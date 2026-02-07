{-# LANGUAGE OverloadedStrings #-}

module Parser.Core
  ( SQLParser(..)
  , mkParser
  , parse
  , execute
  ) where

import Prelude hiding (Left, Right)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.Char (toUpper)
import Text.Read (readMaybe)

import Parser.Types

-- | The SQL parser holds the data to query against
data SQLParser = SQLParser
  { parserData :: [Row]
  } deriving (Show)

mkParser :: [Row] -> SQLParser
mkParser = SQLParser

-- | Parse a SQL query string into a SelectQuery AST
parse :: SQLParser -> Text -> Either Text SelectQuery
parse _ query = do
  let tokens = tokenize query
  parseSelect tokens

-- | Execute a parsed query against the data
execute :: SQLParser -> SelectQuery -> Either Text [Row]
execute p sq = do
  let results0 = parserData p

  -- Apply WHERE clause
  let results1 = case sqWhere sq of
        Nothing   -> results0
        Just cond -> filter (evaluateCondition cond) results0

  -- Apply column selection
  let results2 = if sqColumns sq /= ["*"]
        then map (projectColumns (sqColumns sq)) results1
        else results1

  -- Apply LIMIT
  let results3 = case sqLimit sq of
        Nothing -> results2
        Just n  -> take n results2

  pure results3

-- | Tokenize a SQL query string
tokenize :: Text -> [Text]
tokenize query =
  let q = T.replace "(" " ( "
         $ T.replace ")" " ) "
         $ T.replace "," " , "
         $ T.stripEnd
         $ T.stripSuffix ";" query `orElse` query
  in filter (not . T.null) $ T.words q
  where
    orElse Nothing  x = x
    orElse (Just x) _ = x

-- | Parse the SELECT statement from tokens
parseSelect :: [Text] -> Either Text SelectQuery
parseSelect [] = Left "query must start with SELECT"
parseSelect tokens
  | T.toUpper (head tokens) /= "SELECT" = Left "query must start with SELECT"
  | otherwise = do
      -- Find FROM index
      let fromIdx = findIndex (\t -> T.toUpper t == "FROM") tokens
      case fromIdx of
        Nothing -> Left "expected FROM clause"
        Just fi -> do
          when (fi <= 1) $ Left "no columns specified in SELECT"

          let columns = parseColumns (take (fi - 1) (drop 1 tokens))
          when (null columns || columns == [""]) $ Left "no columns specified in SELECT"

          when (fi + 1 >= length tokens) $ Left "expected table name after FROM"
          let table = tokens !! (fi + 1)

          let currentIdx = fi + 2

          -- Parse WHERE
          (whereCond, idx1) <-
            if currentIdx < length tokens && T.toUpper (tokens !! currentIdx) == "WHERE"
              then do
                (cond, nextIdx) <- parseConditionRecursive tokens (currentIdx + 1)
                pure (Just cond, nextIdx)
              else pure (Nothing, currentIdx)

          -- Parse LIMIT
          (limitVal, _idx2) <-
            if idx1 < length tokens && T.toUpper (tokens !! idx1) == "LIMIT"
              then do
                when (idx1 + 1 >= length tokens) $ Left "expected number after LIMIT"
                let limitToken = tokens !! (idx1 + 1)
                case readMaybe (T.unpack limitToken) :: Maybe Int of
                  Just n | n >= 0 -> pure (Just n, idx1 + 2)
                  _               -> Left "LIMIT must be a non-negative integer"
              else pure (Nothing, idx1)

          pure SelectQuery
            { sqType    = "SELECT"
            , sqColumns = columns
            , sqTable   = table
            , sqWhere   = whereCond
            , sqLimit   = limitVal
            }

-- | Parse column list
parseColumns :: [Text] -> [Text]
parseColumns = filter (not . T.null) . map (T.strip . T.dropAround (== ',')) . filter (/= ",")

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
  | idx + 2 >= length tokens = Left "incomplete condition"
  | otherwise = do
      let leftTok  = tokens !! idx
          opTok    = tokens !! (idx + 1)
          rightTok = tokens !! (idx + 2)
          validOps = ["=", "!=", "<", ">", "AND", "OR"] :: [Text]

      when (opTok `notElem` validOps) $
        Left $ "missing or invalid operator in condition near '" <> leftTok <> "'"

      when (rightTok `elem` validOps) $
        Left $ "unexpected operator '" <> rightTok <> "' after operator '" <> opTok <> "'"

      op <- case opTok of
        "="  -> pure OpEqual
        "!=" -> pure OpNotEqual
        "<"  -> pure OpLess
        ">"  -> pure OpGreater
        _    -> Left $ "invalid operator: " <> opTok

      pure (Condition (COLiteral leftTok) op (COLiteral rightTok), idx + 3)

-- | Evaluate a condition against a row
evaluateCondition :: Condition -> Row -> Bool
evaluateCondition (Condition left op right) row
  | op == OpAnd || op == OpOr =
      case (left, right) of
        (COCondition lc, COCondition rc) ->
          let l = evaluateCondition lc row
              r = evaluateCondition rc row
          in case op of
            OpAnd -> l && r
            OpOr  -> l || r
            _     -> False
        _ -> False
  | otherwise =
      let leftVal  = resolveOperand left row
          rightVal = resolveOperand right row
      in case op of
        OpEqual    -> compareValues leftVal rightVal == 0
        OpNotEqual -> compareValues leftVal rightVal /= 0
        OpLess     -> compareValues leftVal rightVal < 0
        OpGreater  -> compareValues leftVal rightVal > 0
        _          -> False

-- | Resolve an operand to a value (check column names, string literals, numbers)
resolveOperand :: ConditionOperand -> Row -> Value
resolveOperand (COLiteral s) row =
  case HM.lookup s row of
    Just v  -> v
    Nothing
      | T.isPrefixOf "'" s && T.isSuffixOf "'" s && T.length s >= 2
        -> VString (T.drop 1 (T.dropEnd 1 s))
      | otherwise -> case readMaybe (T.unpack s) :: Maybe Double of
          Just n  -> VNumber n
          Nothing -> VString s
resolveOperand (COCondition _) _ = VNull

-- | Compare two values: returns -1, 0, or 1
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

-- | Project specific columns from a row
projectColumns :: [Text] -> Row -> Row
projectColumns cols row = HM.filterWithKey (\k _ -> k `elem` cols) row

-- Helpers
findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex _ [] = Nothing
findIndex p (x:xs)
  | p x       = Just 0
  | otherwise  = fmap (+1) (findIndex p xs)

when :: Bool -> Either Text () -> Either Text ()
when True  e = e
when False _ = pure ()
