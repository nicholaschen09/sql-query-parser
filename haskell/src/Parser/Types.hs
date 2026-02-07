{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.Types
  ( Value(..)
  , Row
  , Operator(..)
  , Condition(..)
  , ConditionOperand(..)
  , ColumnExpr(..)
  , JoinClause(..)
  , OrderByClause(..)
  , SelectQuery(..)
  , QueryResult(..)
  , toNumber
  ) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import GHC.Generics

-- | A single value in a row
data Value
  = VNumber Double
  | VString Text
  | VBool Bool
  | VNull
  | VObject (HashMap Text Value)
  | VArray [Value]
  deriving (Show, Eq, Generic)

instance ToJSON Value where
  toJSON (VNumber n)
    | isInteger' n = toJSON (round n :: Int)
    | otherwise    = toJSON n
    where
      isInteger' x = x == fromIntegral (round x :: Int)
  toJSON (VString s) = toJSON s
  toJSON (VBool b)   = toJSON b
  toJSON VNull       = Null
  toJSON (VObject m) = toJSON m
  toJSON (VArray a)  = toJSON a

instance FromJSON Value where
  parseJSON (Number n) = pure $ VNumber (realToFrac n)
  parseJSON (String s) = pure $ VString s
  parseJSON (Bool b)   = pure $ VBool b
  parseJSON Null       = pure VNull
  parseJSON (Object o) = VObject <$> mapM parseJSON o
  parseJSON (Array a)  = VArray <$> mapM parseJSON (foldr (:) [] a)
  parseJSON _          = fail "unsupported value type"

-- | A row of data: column name -> value
type Row = HashMap Text Value

-- | Operators for conditions
data Operator
  = OpEqual
  | OpNotEqual
  | OpLess
  | OpGreater
  | OpLessEq
  | OpGreaterEq
  | OpAnd
  | OpOr
  | OpLike
  | OpNotLike
  | OpIn
  | OpNotIn
  | OpIs
  | OpIsNot
  deriving (Show, Eq)

instance ToJSON Operator where
  toJSON OpEqual    = String "="
  toJSON OpNotEqual = String "!="
  toJSON OpLess     = String "<"
  toJSON OpGreater  = String ">"
  toJSON OpLessEq   = String "<="
  toJSON OpGreaterEq = String ">="
  toJSON OpAnd      = String "AND"
  toJSON OpOr       = String "OR"
  toJSON OpLike     = String "LIKE"
  toJSON OpNotLike  = String "NOT LIKE"
  toJSON OpIn       = String "IN"
  toJSON OpNotIn    = String "NOT IN"
  toJSON OpIs       = String "IS"
  toJSON OpIsNot    = String "IS NOT"

-- | An operand in a condition
data ConditionOperand
  = COLiteral Text
  | COCondition Condition
  | COValueList [Value]
  | COSubQuery SelectQuery
  | CONull
  deriving (Show, Eq)

instance ToJSON ConditionOperand where
  toJSON (COLiteral t)    = toJSON t
  toJSON (COCondition c)  = toJSON c
  toJSON (COValueList vs) = toJSON vs
  toJSON (COSubQuery _)   = String "<subquery>"
  toJSON CONull           = Null

-- | A WHERE condition node
data Condition = Condition
  { condLeft     :: ConditionOperand
  , condOperator :: Operator
  , condRight    :: ConditionOperand
  } deriving (Show, Eq)

instance ToJSON Condition where
  toJSON (Condition l o r) = object
    [ "left"     .= l
    , "operator" .= o
    , "right"    .= r
    ]

-- | A column expression
data ColumnExpr = ColumnExpr
  { ceIsAggregate :: Bool
  , ceName        :: Text
  , ceFunc        :: Text
  , ceAlias       :: Text
  } deriving (Show, Eq)

-- | A JOIN clause
data JoinClause = JoinClause
  { jcJoinType :: Text
  , jcTable    :: Text
  , jcOn       :: Condition
  } deriving (Show, Eq)

-- | ORDER BY clause
data OrderByClause = OrderByClause
  { obColumn    :: Text
  , obDirection :: Text
  } deriving (Show, Eq)

-- | A parsed SELECT query (the AST)
data SelectQuery = SelectQuery
  { sqType       :: Text
  , sqColumnExprs :: [ColumnExpr]
  , sqTable      :: Text
  , sqJoins      :: [JoinClause]
  , sqWhere      :: Maybe Condition
  , sqGroupBy    :: [Text]
  , sqHaving     :: Maybe Condition
  , sqOrderBy    :: [OrderByClause]
  , sqLimit      :: Maybe Int
  } deriving (Show, Eq)

instance ToJSON SelectQuery where
  toJSON sq = object $
    [ "type"    .= sqType sq
    , "columns" .= map ceName (sqColumnExprs sq)
    , "table"   .= sqTable sq
    ] ++ maybe [] (\w -> ["where" .= w]) (sqWhere sq)
      ++ maybe [] (\l -> ["limit" .= l]) (sqLimit sq)

-- | Query result
data QueryResult = QueryResult
  { qrSuccess :: Bool
  , qrData    :: Maybe [Row]
  , qrError   :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON QueryResult where
  toJSON (QueryResult s d e) = object $
    [ "success" .= s ]
    ++ maybe [] (\dat -> ["data" .= dat]) d
    ++ maybe [] (\err -> ["error" .= err]) e

-- | Try to convert a Value to a number
toNumber :: Value -> Maybe Double
toNumber (VNumber n) = Just n
toNumber (VString s) = case reads (T.unpack s) of
  [(n, "")] -> Just n
  _         -> Nothing
toNumber _ = Nothing
