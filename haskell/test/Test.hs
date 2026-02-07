{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)

import Parser.Types
import Parser.Core

testData :: [Row]
testData =
  [ HM.fromList
      [ ("state", VString "California")
      , ("region", VString "West")
      , ("pop", VNumber 10000)
      , ("pop_male", VNumber 6000)
      , ("pop_female", VNumber 4000)
      ]
  , HM.fromList
      [ ("state", VString "Texas")
      , ("region", VString "South")
      , ("pop", VNumber 5000)
      , ("pop_male", VNumber 2500)
      , ("pop_female", VNumber 2500)
      ]
  , HM.fromList
      [ ("state", VString "Illinois")
      , ("region", VString "Midwest")
      , ("pop", VNumber 2000)
      , ("pop_male", VNumber 1200)
      , ("pop_female", VNumber 800)
      ]
  ]

getState :: Row -> Value
getState row = HM.lookupDefault VNull "state" row

getStates :: [Row] -> [Value]
getStates = map getState

testSelectAll :: Assertion
testSelectAll = do
  let p = mkParser testData
  case parse p "SELECT * FROM table" of
    Left err -> assertFailure $ "Parse error: " ++ show err
    Right q  -> case execute p q of
      Left err  -> assertFailure $ "Execute error: " ++ show err
      Right res -> do
        assertEqual "should return 3 rows" 3 (length res)
        assertEqual "first state" (VString "California") (getState (head res))

testSelectColumns :: Assertion
testSelectColumns = do
  let p = mkParser testData
  case parse p "SELECT state, pop FROM table" of
    Left err -> assertFailure $ "Parse error: " ++ show err
    Right q  -> case execute p q of
      Left err  -> assertFailure $ "Execute error: " ++ show err
      Right res -> do
        assertEqual "should return 3 rows" 3 (length res)
        assertEqual "first state" (VString "California") (getState (head res))
        assertEqual "first pop" (VNumber 10000) (HM.lookupDefault VNull "pop" (head res))
        assertBool "region excluded" (not $ HM.member "region" (head res))

testWhereAnd :: Assertion
testWhereAnd = do
  let p = mkParser testData
  case parse p "SELECT state FROM table WHERE pop > 3000 AND region = 'West'" of
    Left err -> assertFailure $ "Parse error: " ++ show err
    Right q  -> case execute p q of
      Left err  -> assertFailure $ "Execute error: " ++ show err
      Right res -> do
        assertEqual "should return 1 row" 1 (length res)
        assertEqual "state" (VString "California") (getState (head res))

testWhereOrParens :: Assertion
testWhereOrParens = do
  let p = mkParser testData
  case parse p "SELECT state FROM table WHERE pop < 3000 OR (region = 'West' AND pop > 500)" of
    Left err -> assertFailure $ "Parse error: " ++ show err
    Right q  -> case execute p q of
      Left err  -> assertFailure $ "Execute error: " ++ show err
      Right res -> do
        assertEqual "should return 2 rows" 2 (length res)
        let states = getStates res
        assertBool "contains California" (VString "California" `elem` states)
        assertBool "contains Illinois" (VString "Illinois" `elem` states)

testColumnComparison :: Assertion
testColumnComparison = do
  let p = mkParser testData
  case parse p "SELECT state FROM table WHERE pop_male > pop_female" of
    Left err -> assertFailure $ "Parse error: " ++ show err
    Right q  -> case execute p q of
      Left err  -> assertFailure $ "Execute error: " ++ show err
      Right res -> do
        assertEqual "should return 2 rows" 2 (length res)
        let states = getStates res
        assertBool "contains California" (VString "California" `elem` states)
        assertBool "contains Illinois" (VString "Illinois" `elem` states)

testNoMatches :: Assertion
testNoMatches = do
  let p = mkParser testData
  case parse p "SELECT state FROM table WHERE pop > 99999" of
    Left err -> assertFailure $ "Parse error: " ++ show err
    Right q  -> case execute p q of
      Left err  -> assertFailure $ "Execute error: " ++ show err
      Right res -> assertEqual "should return 0 rows" 0 (length res)

testInvalidSql :: Assertion
testInvalidSql = do
  let p = mkParser testData
  case parse p "SELECT FROM table" of
    Left _  -> return ()
    Right _ -> assertFailure "expected error for SELECT FROM table"
  case parse p "SELECT state table" of
    Left _  -> return ()
    Right _ -> assertFailure "expected error for missing FROM"
  case parse p "SELECT state FROM" of
    Left _  -> return ()
    Right _ -> assertFailure "expected error for missing table name"

testLimit :: Assertion
testLimit = do
  let p = mkParser testData
  case parse p "SELECT * FROM table LIMIT 2" of
    Left err -> assertFailure $ "Parse error: " ++ show err
    Right q  -> case execute p q of
      Left err  -> assertFailure $ "Execute error: " ++ show err
      Right res -> assertEqual "should return 2 rows" 2 (length res)

-- New feature tests

testNullSupport :: Assertion
testNullSupport = do
  let users = [ HM.fromList [("name", VString "Alice"), ("age", VNumber 30)]
              , HM.fromList [("name", VString "Bob"), ("age", VNumber 25)]
              , HM.fromList [("name", VString "Charlie"), ("age", VNull)]
              ]
  let p = mkParserWithTables (HM.fromList [("users", users)])
  -- IS NULL
  case parse p "SELECT name FROM users WHERE age IS NULL" of
    Left err -> assertFailure $ "Parse error: " ++ show err
    Right q  -> case execute p q of
      Left err  -> assertFailure $ "Execute error: " ++ show err
      Right res -> do
        assertEqual "IS NULL should return 1 row" 1 (length res)
        assertEqual "name" (VString "Charlie") (HM.lookupDefault VNull "name" (head res))
  -- IS NOT NULL
  case parse p "SELECT name FROM users WHERE age IS NOT NULL" of
    Left err -> assertFailure $ "Parse error: " ++ show err
    Right q  -> case execute p q of
      Left err  -> assertFailure $ "Execute error: " ++ show err
      Right res -> assertEqual "IS NOT NULL should return 2 rows" 2 (length res)

testGroupByCount :: Assertion
testGroupByCount = do
  let orders = [ HM.fromList [("product", VString "Widget"), ("amount", VNumber 50)]
               , HM.fromList [("product", VString "Gadget"), ("amount", VNumber 100)]
               , HM.fromList [("product", VString "Widget"), ("amount", VNumber 30)]
               ]
  let p = mkParserWithTables (HM.fromList [("orders", orders)])
  case parse p "SELECT product, COUNT(*) AS cnt FROM orders GROUP BY product" of
    Left err -> assertFailure $ "Parse error: " ++ show err
    Right q  -> case execute p q of
      Left err  -> assertFailure $ "Execute error: " ++ show err
      Right res -> assertEqual "should return 2 groups" 2 (length res)

testOrderByDesc :: Assertion
testOrderByDesc = do
  let p = mkParser testData
  case parse p "SELECT state, pop FROM table ORDER BY pop DESC" of
    Left err -> assertFailure $ "Parse error: " ++ show err
    Right q  -> case execute p q of
      Left err  -> assertFailure $ "Execute error: " ++ show err
      Right res -> assertEqual "first should be California" (VString "California") (getState (head res))

testComparisonOps :: Assertion
testComparisonOps = do
  let p = mkParser testData
  case parse p "SELECT state FROM table WHERE pop >= 5000" of
    Left err -> assertFailure $ "Parse error: " ++ show err
    Right q  -> case execute p q of
      Left err  -> assertFailure $ "Execute error: " ++ show err
      Right res -> assertEqual ">= 5000 should return 2" 2 (length res)
  case parse p "SELECT state FROM table WHERE pop <= 5000" of
    Left err -> assertFailure $ "Parse error: " ++ show err
    Right q  -> case execute p q of
      Left err  -> assertFailure $ "Execute error: " ++ show err
      Right res -> assertEqual "<= 5000 should return 2" 2 (length res)

testLike :: Assertion
testLike = do
  let users = [ HM.fromList [("name", VString "Alice")]
              , HM.fromList [("name", VString "Bob")]
              ]
  let p = mkParserWithTables (HM.fromList [("users", users)])
  case parse p "SELECT name FROM users WHERE name LIKE 'A%'" of
    Left err -> assertFailure $ "Parse error: " ++ show err
    Right q  -> case execute p q of
      Left err  -> assertFailure $ "Execute error: " ++ show err
      Right res -> do
        assertEqual "LIKE A% should return 1" 1 (length res)
        assertEqual "name" (VString "Alice") (HM.lookupDefault VNull "name" (head res))

testInList :: Assertion
testInList = do
  let users = [ HM.fromList [("name", VString "Alice"), ("age", VNumber 30)]
              , HM.fromList [("name", VString "Bob"), ("age", VNumber 25)]
              ]
  let p = mkParserWithTables (HM.fromList [("users", users)])
  case parse p "SELECT name FROM users WHERE age IN (25, 30)" of
    Left err -> assertFailure $ "Parse error: " ++ show err
    Right q  -> case execute p q of
      Left err  -> assertFailure $ "Execute error: " ++ show err
      Right res -> assertEqual "IN (25, 30) should return 2" 2 (length res)

main :: IO ()
main = defaultMain
  [ testCase "selects all rows with *" testSelectAll
  , testCase "selects specific columns" testSelectColumns
  , testCase "filters with WHERE and AND" testWhereAnd
  , testCase "filters with OR and parentheses" testWhereOrParens
  , testCase "supports column-to-column comparison" testColumnComparison
  , testCase "returns empty for no matches" testNoMatches
  , testCase "throws on invalid SQL" testInvalidSql
  , testCase "supports LIMIT" testLimit
  , testCase "supports NULL (IS NULL / IS NOT NULL)" testNullSupport
  , testCase "supports GROUP BY with COUNT" testGroupByCount
  , testCase "supports ORDER BY DESC" testOrderByDesc
  , testCase "supports >= and <=" testComparisonOps
  , testCase "supports LIKE" testLike
  , testCase "supports IN value list" testInList
  ]
