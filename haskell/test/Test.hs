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
  ]
