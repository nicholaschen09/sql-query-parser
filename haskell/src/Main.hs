{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (FromJSON(..), eitherDecode, encode, object, (.=), (.:), (.:?), withObject)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Web.Scotty
import System.Environment (lookupEnv)
import Network.Wai.Middleware.AddHeaders (addHeaders)

import Parser.Types (Row, QueryResult(..))
import Parser.Core (mkParser, mkParserWithTables)
import qualified Parser.Core as P

data ExecuteRequest = ExecuteRequest
  { erQuery  :: Text
  , erData   :: Maybe [Row]
  , erTables :: Maybe (HM.HashMap Text [Row])
  }

instance FromJSON ExecuteRequest where
  parseJSON = withObject "ExecuteRequest" $ \v ->
    ExecuteRequest <$> v .: "query" <*> v .:? "data" <*> v .:? "tables"

main :: IO ()
main = do
  portStr <- lookupEnv "PORT"
  let port = maybe 8082 read portStr :: Int
  putStrLn $ "Haskell SQL Parser server starting on port " ++ show port

  scotty port $ do
    middleware $ addHeaders
      [ ("Access-Control-Allow-Origin", "*")
      , ("Access-Control-Allow-Methods", "POST, GET, OPTIONS")
      , ("Access-Control-Allow-Headers", "Content-Type")
      ]

    options (regex ".*") $ do
      text ""

    get "/health" $ do
      setHeader "Content-Type" "application/json"
      raw $ encode $ object ["status" .= ("ok" :: Text)]

    post "/execute" $ do
      body' <- body
      case eitherDecode body' of
        Left err -> do
          setHeader "Content-Type" "application/json"
          raw $ encode $ QueryResult False Nothing (Just $ T.pack $ "Invalid request: " ++ err)
        Right req -> do
          let parser = case erTables req of
                Just tables -> mkParserWithTables tables
                Nothing     -> case erData req of
                  Just d  -> mkParser d
                  Nothing -> mkParser []
          case P.parse parser (erQuery req) of
            Left parseErr -> do
              setHeader "Content-Type" "application/json"
              raw $ encode $ QueryResult False Nothing (Just parseErr)
            Right sq ->
              case P.execute parser sq of
                Left execErr -> do
                  setHeader "Content-Type" "application/json"
                  raw $ encode $ QueryResult False Nothing (Just execErr)
                Right results -> do
                  setHeader "Content-Type" "application/json"
                  raw $ encode $ QueryResult True (Just results) Nothing
