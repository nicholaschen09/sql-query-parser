{-# LANGUAGE OverloadedStrings #-}

module Parser.Core
  ( SQLParser(..)
  , mkParser
  , mkParserWithTables
  , parse
  , execute
  ) where

import Prelude hiding (Left, Right)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.Char (toUpper, isSpace)
import Text.Read (readMaybe)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe, mapMaybe)

import Parser.Types

-- | The SQL parser holds table data
data SQLParser = SQLParser
  { parserTables :: HM.HashMap Text [Row]
  } deriving (Show)

mkParser :: [Row] -> SQLParser
mkParser rows = SQLParser (HM.singleton "table" rows)

mkParserWithTables :: HM.HashMap Text [Row] -> SQLParser
mkParserWithTables = SQLParser

-- | Parse a SQL query string into a SelectQuery AST
parse :: SQLParser -> Text -> Either Text SelectQuery
parse _ query = do
  let tokens = tokenize query
  parseSelect tokens

-- | Execute a parsed query against the data
execute :: SQLParser -> SelectQuery -> Either Text [Row]
execute p sq = do
  -- 1. Get base table
  let tableName = sqTable sq
  results0 <- case getTableData p tableName of
    Nothing -> Left $ "table '" <> tableName <> "' not found"
    Just d  -> Right d

  -- 2. JOINs
  results1 <- if null (sqJoins sq)
    then Right results0
    else executeJoins p results0 tableName (sqJoins sq)

  -- 3. WHERE
  let results2 = case sqWhere sq of
        Nothing   -> results1
        Just cond -> filter (evaluateCondition p cond) results1

  -- 4. GROUP BY + aggregations
  let cols = sqColumnExprs sq
      hasAgg = any ceIsAggregate cols
  let results3 = if not (null (sqGroupBy sq)) || hasAgg
        then executeGroupBy p results2 (sqGroupBy sq) cols (sqHaving sq)
        else if not (null cols) && not (length cols == 1 && not (ceIsAggregate (head cols)) && ceName (head cols) == "*")
          then map (projectColumns p cols) results2
          else results2

  -- 5. ORDER BY
  let results4 = if null (sqOrderBy sq)
        then results3
        else executeOrderBy p results3 (sqOrderBy sq)

  -- 6. LIMIT
  let results5 = case sqLimit sq of
        Nothing -> results4
        Just n  -> take n results4

  pure results5

-- | Tokenize a SQL query string
tokenize :: Text -> [Text]
tokenize query = tokenizeLoop (T.stripEnd $ fromMaybe q (T.stripSuffix ";" q)) []
  where
    q = T.strip query

tokenizeLoop :: Text -> [Text] -> [Text]
tokenizeLoop t acc
  | T.null t = reverse acc
  | isSpace (T.head t) = tokenizeLoop (T.tail t) acc
  -- String literal
  | T.head t == '\'' =
      let rest = T.tail t
          (inside, after) = T.break (== '\'') rest
          token = "'" <> inside <> "'"
          remaining = if T.null after then T.empty else T.tail after
      in tokenizeLoop remaining (token : acc)
  -- Two-char operators
  | T.length t >= 2 && T.take 2 t `elem` [">=", "<=", "!="] =
      tokenizeLoop (T.drop 2 t) (T.take 2 t : acc)
  -- Single-char punctuation
  | T.head t `elem` ("(),=<>*" :: String) =
      tokenizeLoop (T.tail t) (T.singleton (T.head t) : acc)
  -- Words
  | otherwise =
      let (word, rest) = T.break (\c -> isSpace c || c `elem` ("(),=<>*'" :: String) || (c == '!' && T.length rest' > 0 && T.head rest' == '=')) t
          rest' = T.drop (T.length word) t
      in if T.null word
         then tokenizeLoop (T.tail t) acc
         else tokenizeLoop rest' (word : acc)

-- | Parse SELECT
parseSelect :: [Text] -> Either Text SelectQuery
parseSelect [] = Left "query must start with SELECT"
parseSelect tokens
  | T.toUpper (head tokens) /= "SELECT" = Left "query must start with SELECT"
  | otherwise = do
      let fromIdx = findFromIndex tokens 1 0
      case fromIdx of
        Nothing -> Left "expected FROM clause"
        Just fi -> do
          when' (fi <= 1) $ Left "no columns specified in SELECT"
          let colExprs = parseColumnExprs (take (fi - 1) (drop 1 tokens))
          when' (null colExprs) $ Left "no columns specified in SELECT"
          when' (fi + 1 >= length tokens) $ Left "expected table name after FROM"
          let table = tokens !! (fi + 1)
          let idx0 = fi + 2

          -- JOINs
          (joins, idx1) <- parseJoins tokens idx0

          -- WHERE
          (whereCond, idx2) <-
            if idx1 < length tokens && T.toUpper (tokens !! idx1) == "WHERE"
              then do
                (cond, nextIdx) <- parseConditionRecursive tokens (idx1 + 1)
                pure (Just cond, nextIdx)
              else pure (Nothing, idx1)

          -- GROUP BY
          (groupBy, idx3) <-
            if idx2 < length tokens && T.toUpper (tokens !! idx2) == "GROUP"
               && idx2 + 1 < length tokens && T.toUpper (tokens !! (idx2 + 1)) == "BY"
              then do
                let (gb, nextIdx) = parseGroupByList tokens (idx2 + 2)
                pure (gb, nextIdx)
              else pure ([], idx2)

          -- HAVING
          (havingCond, idx4) <-
            if idx3 < length tokens && T.toUpper (tokens !! idx3) == "HAVING"
              then do
                (cond, nextIdx) <- parseConditionRecursive tokens (idx3 + 1)
                pure (Just cond, nextIdx)
              else pure (Nothing, idx3)

          -- ORDER BY
          (orderBy, idx5) <-
            if idx4 < length tokens && T.toUpper (tokens !! idx4) == "ORDER"
               && idx4 + 1 < length tokens && T.toUpper (tokens !! (idx4 + 1)) == "BY"
              then do
                let (ob, nextIdx) = parseOrderByList tokens (idx4 + 2)
                pure (ob, nextIdx)
              else pure ([], idx4)

          -- LIMIT
          (limitVal, _idx6) <-
            if idx5 < length tokens && T.toUpper (tokens !! idx5) == "LIMIT"
              then do
                when' (idx5 + 1 >= length tokens) $ Left "expected number after LIMIT"
                let limitToken = tokens !! (idx5 + 1)
                case readMaybe (T.unpack limitToken) :: Maybe Int of
                  Just n | n >= 0 -> pure (Just n, idx5 + 2)
                  _               -> Left "LIMIT must be a non-negative integer"
              else pure (Nothing, idx5)

          pure SelectQuery
            { sqType       = "SELECT"
            , sqColumnExprs = colExprs
            , sqTable      = table
            , sqJoins      = joins
            , sqWhere      = whereCond
            , sqGroupBy    = groupBy
            , sqHaving     = havingCond
            , sqOrderBy    = orderBy
            , sqLimit      = limitVal
            }

findFromIndex :: [Text] -> Int -> Int -> Maybe Int
findFromIndex tokens idx depth
  | idx >= length tokens = Nothing
  | tokens !! idx == "(" = findFromIndex tokens (idx + 1) (depth + 1)
  | tokens !! idx == ")" = findFromIndex tokens (idx + 1) (depth - 1)
  | depth == 0 && T.toUpper (tokens !! idx) == "FROM" = Just idx
  | otherwise = findFromIndex tokens (idx + 1) depth

parseColumnExprs :: [Text] -> [ColumnExpr]
parseColumnExprs = parseColLoop []
  where
    aggFuncs = ["COUNT", "SUM", "AVG", "MIN", "MAX"]
    parseColLoop acc [] = reverse acc
    parseColLoop acc (t:ts)
      | t == "," || T.null t = parseColLoop acc ts
      | T.toUpper t `elem` aggFuncs = parseAgg acc t ts
      | otherwise = parseColLoop (ColumnExpr False (T.strip $ T.dropAround (== ',') t) "" "" : acc) ts

    parseAgg acc funcName rest = case rest of
      ("(":rs) ->
        let (inside, afterParen) = break (== ")") rs
            col = T.intercalate " " inside
            rest2 = drop 1 afterParen  -- skip )
        in case rest2 of
          ("AS":aliasName:rest3) ->
            parseColLoop (ColumnExpr True (if T.null col then "*" else col) (T.toUpper funcName) aliasName : acc) rest3
          _ ->
            parseColLoop (ColumnExpr True (if T.null col then "*" else col) (T.toUpper funcName) "" : acc) rest2
      _ -> parseColLoop (ColumnExpr False funcName "" "" : acc) rest

parseJoins :: [Text] -> Int -> Either Text ([JoinClause], Int)
parseJoins tokens = go []
  where
    go acc idx
      | idx >= length tokens = Right (reverse acc, idx)
      | otherwise =
          let upper = T.toUpper (tokens !! idx)
          in case () of
            _ | upper == "JOIN" -> parseJoinBody acc "INNER" (idx + 1)
              | upper == "INNER" && idx + 1 < length tokens && T.toUpper (tokens !! (idx + 1)) == "JOIN" ->
                  parseJoinBody acc "INNER" (idx + 2)
              | upper == "LEFT" ->
                  let idx' = if idx + 1 < length tokens && T.toUpper (tokens !! (idx + 1)) == "JOIN"
                             then idx + 2 else idx + 1
                  in parseJoinBody acc "LEFT" idx'
              | upper == "RIGHT" ->
                  let idx' = if idx + 1 < length tokens && T.toUpper (tokens !! (idx + 1)) == "JOIN"
                             then idx + 2 else idx + 1
                  in parseJoinBody acc "RIGHT" idx'
              | otherwise -> Right (reverse acc, idx)

    parseJoinBody acc joinType idx = do
      when' (idx >= length tokens) $ Left "expected table name after JOIN"
      let table = tokens !! idx
      when' (idx + 1 >= length tokens || T.toUpper (tokens !! (idx + 1)) /= "ON") $
        Left "expected ON after JOIN table name"
      (cond, nextIdx) <- parseConditionRecursive tokens (idx + 2)
      go (JoinClause joinType table cond : acc) nextIdx

parseGroupByList :: [Text] -> Int -> ([Text], Int)
parseGroupByList tokens = go []
  where
    go acc idx
      | idx >= length tokens = (reverse acc, idx)
      | let u = T.toUpper (tokens !! idx)
        in u == "HAVING" || u == "ORDER" || u == "LIMIT" = (reverse acc, idx)
      | tokens !! idx == "," = go acc (idx + 1)
      | otherwise = go (tokens !! idx : acc) (idx + 1)

parseOrderByList :: [Text] -> Int -> ([OrderByClause], Int)
parseOrderByList tokens = go []
  where
    go acc idx
      | idx >= length tokens = (reverse acc, idx)
      | T.toUpper (tokens !! idx) == "LIMIT" = (reverse acc, idx)
      | tokens !! idx == "," = go acc (idx + 1)
      | otherwise =
          let col = tokens !! idx
              (dir, idx') = if idx + 1 < length tokens
                            then let d = T.toUpper (tokens !! (idx + 1))
                                 in if d == "ASC" || d == "DESC"
                                    then (d, idx + 2)
                                    else ("ASC", idx + 1)
                            else ("ASC", idx + 1)
          in go (OrderByClause col dir : acc) idx'

-- | Parse OR (lowest precedence)
parseConditionRecursive :: [Text] -> Int -> Either Text (Condition, Int)
parseConditionRecursive tokens idx = do
  (left, nextIdx) <- parseAnd tokens idx
  parseOrLoop tokens left nextIdx

parseOrLoop :: [Text] -> Condition -> Int -> Either Text (Condition, Int)
parseOrLoop tokens left nextIdx
  | nextIdx < length tokens && T.toUpper (tokens !! nextIdx) == "OR" = do
      (right, afterRight) <- parseAnd tokens (nextIdx + 1)
      let combined = Condition (COCondition left) OpOr (COCondition right)
      parseOrLoop tokens combined afterRight
  | otherwise = pure (left, nextIdx)

-- | Parse AND (higher precedence)
parseAnd :: [Text] -> Int -> Either Text (Condition, Int)
parseAnd tokens idx = do
  (left, nextIdx) <- parseOperand tokens idx
  parseAndLoop tokens left nextIdx

parseAndLoop :: [Text] -> Condition -> Int -> Either Text (Condition, Int)
parseAndLoop tokens left nextIdx
  | nextIdx < length tokens && T.toUpper (tokens !! nextIdx) == "AND" = do
      (right, afterRight) <- parseOperand tokens (nextIdx + 1)
      let combined = Condition (COCondition left) OpAnd (COCondition right)
      parseAndLoop tokens combined afterRight
  | otherwise = pure (left, nextIdx)

-- | Parse a single comparison or parenthesized expression
parseOperand :: [Text] -> Int -> Either Text (Condition, Int)
parseOperand tokens idx
  | idx >= length tokens = Left "unexpected end of tokens"
  | tokens !! idx == "(" = do
      (cond, nextIdx) <- parseConditionRecursive tokens (idx + 1)
      if nextIdx >= length tokens || tokens !! nextIdx /= ")"
        then Left "expected closing parenthesis"
        else pure (cond, nextIdx + 1)
  | otherwise = do
      let leftTok = tokens !! idx
          opIdx0 = idx + 1
      when' (opIdx0 >= length tokens) $ Left $ "incomplete condition near '" <> leftTok <> "'"

      -- IS NULL / IS NOT NULL
      if T.toUpper (tokens !! opIdx0) == "IS"
        then if opIdx0 + 1 < length tokens && T.toUpper (tokens !! (opIdx0 + 1)) == "NOT"
                && opIdx0 + 2 < length tokens && T.toUpper (tokens !! (opIdx0 + 2)) == "NULL"
          then pure (Condition (COLiteral leftTok) OpIsNot CONull, opIdx0 + 3)
          else if opIdx0 + 1 < length tokens && T.toUpper (tokens !! (opIdx0 + 1)) == "NULL"
            then pure (Condition (COLiteral leftTok) OpIs CONull, opIdx0 + 2)
            else parseStandardOp tokens idx leftTok opIdx0 False
        else parseStandardOp tokens idx leftTok opIdx0 False

parseStandardOp :: [Text] -> Int -> Text -> Int -> Bool -> Either Text (Condition, Int)
parseStandardOp tokens _origIdx leftTok opIdx0 _unused = do
  let hasNot = opIdx0 < length tokens && T.toUpper (tokens !! opIdx0) == "NOT"
      opIdx = if hasNot then opIdx0 + 1 else opIdx0

  -- IN
  if opIdx < length tokens && T.toUpper (tokens !! opIdx) == "IN"
    then do
      let opIdx' = opIdx + 1
      when' (opIdx' >= length tokens || tokens !! opIdx' /= "(") $ Left "expected ( after IN"
      let opIdx'' = opIdx' + 1
      -- Value list
      let (values, afterParen) = parseValueList tokens opIdx''
          op = if hasNot then OpNotIn else OpIn
      pure (Condition (COLiteral leftTok) op (COValueList values), afterParen)
    else if opIdx < length tokens && T.toUpper (tokens !! opIdx) == "LIKE"
      then do
        let opIdx' = opIdx + 1
        when' (opIdx' >= length tokens) $ Left "expected pattern after LIKE"
        let op = if hasNot then OpNotLike else OpLike
        pure (Condition (COLiteral leftTok) op (COLiteral (tokens !! opIdx')), opIdx' + 1)
      else do
        -- Standard comparison
        let actualOpIdx = if hasNot then opIdx0 else opIdx  -- put NOT back
        when' (actualOpIdx >= length tokens) $ Left $ "incomplete condition near '" <> leftTok <> "'"
        let opTok = tokens !! actualOpIdx
        op <- case opTok of
          "="  -> pure OpEqual
          "!=" -> pure OpNotEqual
          "<"  -> pure OpLess
          ">"  -> pure OpGreater
          "<=" -> pure OpLessEq
          ">=" -> pure OpGreaterEq
          _    -> Left $ "missing or invalid operator in condition near '" <> leftTok <> "'"
        when' (actualOpIdx + 1 >= length tokens) $
          Left $ "missing right-hand side in condition near '" <> leftTok <> "'"
        let rightTok = tokens !! (actualOpIdx + 1)
            rightOp = if T.toUpper rightTok == "NULL" then CONull else COLiteral rightTok
        pure (Condition (COLiteral leftTok) op rightOp, actualOpIdx + 2)

parseValueList :: [Text] -> Int -> ([Value], Int)
parseValueList tokens = go []
  where
    go acc idx
      | idx >= length tokens = (reverse acc, idx)
      | tokens !! idx == ")" = (reverse acc, idx + 1)
      | tokens !! idx == "," = go acc (idx + 1)
      | otherwise = go (parseValueToken (tokens !! idx) : acc) (idx + 1)

parseValueToken :: Text -> Value
parseValueToken t
  | T.toUpper t == "NULL" = VNull
  | T.isPrefixOf "'" t && T.isSuffixOf "'" t && T.length t >= 2 =
      VString (T.drop 1 (T.dropEnd 1 t))
  | otherwise = case readMaybe (T.unpack t) :: Maybe Double of
      Just n  -> VNumber n
      Nothing -> VString t

-- | Resolve a value from a row (supports dot notation)
resolveValue :: Row -> Text -> Maybe Value
resolveValue row path
  | T.null path = Nothing
  | HM.member path row = HM.lookup path row
  | T.isInfixOf "." path =
      let parts = T.splitOn "." path
      in tryPrefixes row parts 1
  | otherwise = Nothing
  where
    tryPrefixes _ _ i | i >= length (T.splitOn "." path) = Nothing
    tryPrefixes r ps i =
      let prefix = T.intercalate "." (take i ps)
          rest = drop i ps
      in case HM.lookup prefix r of
        Just val -> traverseValue val rest
        Nothing  -> tryPrefixes r ps (i + 1)

    traverseValue v [] = Just v
    traverseValue (VObject m) (p:ps) = case HM.lookup p m of
      Just v  -> traverseValue v ps
      Nothing -> Nothing
    traverseValue _ _ = Nothing

-- | Resolve an operand to a value
resolveOperand :: SQLParser -> Row -> ConditionOperand -> Value
resolveOperand _ row (COLiteral s) =
  case resolveValue row s of
    Just v  -> v
    Nothing
      | T.isPrefixOf "'" s && T.isSuffixOf "'" s && T.length s >= 2
        -> VString (T.drop 1 (T.dropEnd 1 s))
      | otherwise -> case readMaybe (T.unpack s) :: Maybe Double of
          Just n  -> VNumber n
          Nothing -> VString s
resolveOperand _ _ CONull = VNull
resolveOperand _ _ _ = VNull

-- | Evaluate a condition against a row
evaluateCondition :: SQLParser -> Condition -> Row -> Bool
evaluateCondition p (Condition left op right) row
  | op == OpAnd || op == OpOr =
      case (left, right) of
        (COCondition lc, COCondition rc) ->
          let l = evaluateCondition p lc row
              r = evaluateCondition p rc row
          in case op of
            OpAnd -> l && r
            OpOr  -> l || r
            _     -> False
        _ -> False
  | op == OpIs =
      let leftVal = resolveOperand p row left
      in leftVal == VNull
  | op == OpIsNot =
      let leftVal = resolveOperand p row left
      in leftVal /= VNull
  | op == OpIn || op == OpNotIn =
      let leftVal = resolveOperand p row left
      in if leftVal == VNull then False
         else case right of
           COValueList values ->
             let found = any (\v -> compareValues leftVal v == 0) values
             in if op == OpNotIn then not found else found
           _ -> False
  | op == OpLike || op == OpNotLike =
      let leftVal = resolveOperand p row left
      in if leftVal == VNull then False
         else let pattern = case right of
                    COLiteral s ->
                      let s' = if T.isPrefixOf "'" s && T.isSuffixOf "'" s && T.length s >= 2
                               then T.drop 1 (T.dropEnd 1 s) else s
                      in T.replace "%" ".*" (T.replace "_" "." s')
                    _ -> ".*"
                  leftStr = case leftVal of
                    VString s -> s
                    VNumber n -> T.pack (show n)
                    _ -> ""
                  -- Simple pattern match (case insensitive)
                  matches = simpleMatch (T.toLower pattern) (T.toLower leftStr)
              in if op == OpNotLike then not matches else matches
  | otherwise =
      let leftVal  = resolveOperand p row left
          rightVal = resolveOperand p row right
      in if leftVal == VNull || rightVal == VNull
         then case op of
           OpEqual    -> leftVal == VNull && rightVal == VNull
           OpNotEqual -> not (leftVal == VNull && rightVal == VNull)
           _          -> False
         else case op of
           OpEqual    -> compareValues leftVal rightVal == 0
           OpNotEqual -> compareValues leftVal rightVal /= 0
           OpLess     -> compareValues leftVal rightVal < 0
           OpGreater  -> compareValues leftVal rightVal > 0
           OpLessEq   -> compareValues leftVal rightVal <= 0
           OpGreaterEq -> compareValues leftVal rightVal > 0 || compareValues leftVal rightVal == 0
           _          -> False

-- Simple regex-like match for LIKE patterns
simpleMatch :: Text -> Text -> Bool
simpleMatch pattern str
  | T.null pattern && T.null str = True
  | T.isPrefixOf ".*" pattern = 
      -- .* matches any number of characters
      let rest = T.drop 2 pattern
      in any (\i -> simpleMatch rest (T.drop i str)) [0..T.length str]
  | T.isPrefixOf "." pattern && not (T.null str) =
      simpleMatch (T.drop 1 pattern) (T.drop 1 str)
  | not (T.null pattern) && not (T.null str) && T.head pattern == T.head str =
      simpleMatch (T.tail pattern) (T.tail str)
  | otherwise = False

-- | Compare two values
compareValues :: Value -> Value -> Int
compareValues l r =
  case (toNumber l, toNumber r) of
    (Just ln, Just rn)
      | ln < rn   -> -1
      | ln > rn   -> 1
      | otherwise  -> 0
    _ ->
      let ls = showValue l
          rs = showValue r
      in if ls < rs then -1
         else if ls > rs then 1
         else 0

showValue :: Value -> Text
showValue (VNumber n) = T.pack (show n)
showValue (VString s) = s
showValue (VBool b)   = T.pack (show b)
showValue VNull       = "null"
showValue _           = ""

-- | Project columns from a row
projectColumns :: SQLParser -> [ColumnExpr] -> Row -> Row
projectColumns _ cols row = HM.fromList
  [ (ceName c, fromMaybe VNull (resolveValue row (ceName c)))
  | c <- cols, not (ceIsAggregate c)
  ]

-- | Get table data by name
getTableData :: SQLParser -> Text -> Maybe [Row]
getTableData p name =
  case HM.lookup name (parserTables p) of
    Just d  -> Just d
    Nothing ->
      let lower = T.toLower name
      in case filter (\(k, _) -> T.toLower k == lower) (HM.toList (parserTables p)) of
        ((_, d):_) -> Just d
        [] -> case HM.lookup "table" (parserTables p) of
          Just d  -> Just d
          Nothing -> if HM.size (parserTables p) == 1
                     then Just (snd (head (HM.toList (parserTables p))))
                     else Nothing

-- | Execute JOINs
executeJoins :: SQLParser -> [Row] -> Text -> [JoinClause] -> Either Text [Row]
executeJoins _ results _ [] = Right results
executeJoins p results leftTable (join:rest) = do
  rightData <- case getTableData p (jcTable join) of
    Nothing -> Left $ "table '" <> jcTable join <> "' not found for JOIN"
    Just d  -> Right d

  let joined = case jcJoinType join of
        "RIGHT" -> concatMap (\rr ->
          let matches = [mergeRows lr rr leftTable (jcTable join) | lr <- results, evaluateCondition p (jcOn join) (mergeRows lr rr leftTable (jcTable join))]
          in if null matches
             then [mergeRowWithNulls rr (jcTable join) leftTable (if null results then HM.empty else head results)]
             else matches) rightData
        _ -> concatMap (\lr ->
          let matches = [mergeRows lr rr leftTable (jcTable join) | rr <- rightData, evaluateCondition p (jcOn join) (mergeRows lr rr leftTable (jcTable join))]
          in if null matches && jcJoinType join == "LEFT"
             then [mergeRowLeftNull lr leftTable (jcTable join) (if null rightData then HM.empty else head rightData)]
             else matches) results

  executeJoins p joined leftTable rest

mergeRows :: Row -> Row -> Text -> Text -> Row
mergeRows leftRow rightRow leftTable rightTable =
  let withLeft = HM.foldlWithKey' (\m k v ->
        let m' = HM.insert k v m
        in if T.isInfixOf "." k then m' else HM.insert (leftTable <> "." <> k) v m') HM.empty leftRow
  in HM.foldlWithKey' (\m k v ->
        let m' = HM.insert k v m
        in if T.isInfixOf "." k then m' else HM.insert (rightTable <> "." <> k) v m') withLeft rightRow

mergeRowLeftNull :: Row -> Text -> Text -> Row -> Row
mergeRowLeftNull leftRow leftTable rightTable sampleRight =
  let withLeft = HM.foldlWithKey' (\m k v ->
        let m' = HM.insert k v m
        in if T.isInfixOf "." k then m' else HM.insert (leftTable <> "." <> k) v m') HM.empty leftRow
  in HM.foldlWithKey' (\m k _ ->
        let m' = HM.insert k VNull m
        in HM.insert (rightTable <> "." <> k) VNull m') withLeft sampleRight

mergeRowWithNulls :: Row -> Text -> Text -> Row -> Row
mergeRowWithNulls rightRow rightTable leftTable sampleLeft =
  let withRight = HM.foldlWithKey' (\m k v ->
        let m' = HM.insert k v m
        in if T.isInfixOf "." k then m' else HM.insert (rightTable <> "." <> k) v m') HM.empty rightRow
  in HM.foldlWithKey' (\m k _ ->
        let m' = HM.insert k VNull m
        in HM.insert (leftTable <> "." <> k) VNull m') withRight sampleLeft

-- | Execute GROUP BY
executeGroupBy :: SQLParser -> [Row] -> [Text] -> [ColumnExpr] -> Maybe Condition -> [Row]
executeGroupBy p rows groupBy cols havingCond =
  let groups = if null groupBy
        then [("__all__", rows)]
        else groupRows rows groupBy
      results = map (buildGroupRow p groupBy cols) groups
  in case havingCond of
       Nothing -> results
       Just cond -> filter (evaluateCondition p cond) results

groupRows :: [Row] -> [Text] -> [(Text, [Row])]
groupRows rows groupBy = foldl addToGroup [] rows
  where
    addToGroup groups row =
      let key = T.intercalate "|" (map (\col -> showValue (fromMaybe VNull (resolveValue row col))) groupBy)
      in case lookup key groups of
        Just _  -> map (\(k, rs) -> if k == key then (k, rs ++ [row]) else (k, rs)) groups
        Nothing -> groups ++ [(key, [row])]

buildGroupRow :: SQLParser -> [Text] -> [ColumnExpr] -> (Text, [Row]) -> Row
buildGroupRow _ groupBy cols (_, groupRows') =
  let gbValues = HM.fromList [(col, fromMaybe VNull (resolveValue (head groupRows') col)) | col <- groupBy]
      aggValues = HM.fromList $ mapMaybe (\c ->
        if ceIsAggregate c
          then let alias = if T.null (ceAlias c) then ceFunc c <> "(" <> ceName c <> ")" else ceAlias c
               in Just (alias, computeAggregate (ceFunc c) (ceName c) groupRows')
          else if ceName c /= "*" && ceName c `notElem` groupBy
            then Just (ceName c, fromMaybe VNull (resolveValue (head groupRows') (ceName c)))
            else Nothing) cols
  in HM.union gbValues aggValues

computeAggregate :: Text -> Text -> [Row] -> Value
computeAggregate func col rows = case func of
  "COUNT" -> if col == "*"
    then VNumber (fromIntegral (length rows))
    else VNumber (fromIntegral (length (filter (\r -> resolveValue r col /= Just VNull && resolveValue r col /= Nothing) rows)))
  "SUM" ->
    let nums = mapMaybe (\r -> resolveValue r col >>= toNumber) rows
    in VNumber (sum nums)
  "AVG" ->
    let nums = mapMaybe (\r -> resolveValue r col >>= toNumber) rows
    in if null nums then VNull else VNumber (sum nums / fromIntegral (length nums))
  "MIN" ->
    let vals = mapMaybe (\r -> resolveValue r col) (filter (\r -> resolveValue r col /= Just VNull) rows)
    in if null vals then VNull else foldr1 (\a b -> if compareValues a b < 0 then a else b) vals
  "MAX" ->
    let vals = mapMaybe (\r -> resolveValue r col) (filter (\r -> resolveValue r col /= Just VNull) rows)
    in if null vals then VNull else foldr1 (\a b -> if compareValues a b > 0 then a else b) vals
  _ -> VNull

-- | Execute ORDER BY
executeOrderBy :: SQLParser -> [Row] -> [OrderByClause] -> [Row]
executeOrderBy _ rows orderBy = sortBy (compareByOrder orderBy) rows
  where
    compareByOrder [] _ _ = EQ
    compareByOrder (ob:obs) a b =
      let av = fromMaybe VNull (resolveValue a (obColumn ob))
          bv = fromMaybe VNull (resolveValue b (obColumn ob))
          cmp = compareValues av bv
          cmp' = if obDirection ob == "DESC" then negate cmp else cmp
      in if cmp' < 0 then LT
         else if cmp' > 0 then GT
         else compareByOrder obs a b

-- Helpers
when' :: Bool -> Either Text () -> Either Text ()
when' True  e = e
when' False _ = pure ()
