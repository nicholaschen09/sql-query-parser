{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.Types
  ( Value(..)
  , Row
  , Operator(..)
  , Condition(..)
  , ConditionOperand(..)
  , SelectQuery(..)
  , QueryResult(..)
  , toNumber
  ) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import GHC.Generics
import Data.Scientific (toRealFloat, isInteger, toBoundedInteger)

-- | A single value in a row
data Value
  = VNumber Double
  | VString Text
  | VBool Bool
  | VNull
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

instance FromJSON Value where
  parseJSON (Number n) = pure $ VNumber (toRealFloat n)
  parseJSON (String s) = pure $ VString s
  parseJSON (Bool b)   = pure $ VBool b
  parseJSON Null       = pure VNull
  parseJSON _          = fail "unsupported value type"

-- | A row of data: column name -> value
type Row = HashMap Text Value

-- | Operators for conditions
data Operator
  = OpEqual
  | OpNotEqual
  | OpLess
  | OpGreater
  | OpAnd
  | OpOr
  deriving (Show, Eq)

instance ToJSON Operator where
  toJSON OpEqual    = String "="
  toJSON OpNotEqual = String "!="
  toJSON OpLess     = String "<"
  toJSON OpGreater  = String ">"
  toJSON OpAnd      = String "AND"
  toJSON OpOr       = String "OR"

instance FromJSON Operator where
  parseJSON (String "=")   = pure OpEqual
  parseJSON (String "!=")  = pure OpNotEqual
  parseJSON (String "<")   = pure OpLess
  parseJSON (String ">")   = pure OpGreater
  parseJSON (String "AND") = pure OpAnd
  parseJSON (String "OR")  = pure OpOr
  parseJSON _              = fail "invalid operator"

-- | An operand in a condition
data ConditionOperand
  = COLiteral Text
  | COCondition Condition
  deriving (Show, Eq)

instance ToJSON ConditionOperand where
  toJSON (COLiteral t)    = toJSON t
  toJSON (COCondition c)  = toJSON c

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

-- | A parsed SELECT query (the AST)
data SelectQuery = SelectQuery
  { sqType    :: Text
  , sqColumns :: [Text]
  , sqTable   :: Text
  , sqWhere   :: Maybe Condition
  , sqLimit   :: Maybe Int
  } deriving (Show, Eq)

instance ToJSON SelectQuery where
  toJSON sq = object $
    [ "type"    .= sqType sq
    , "columns" .= sqColumns sq
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
