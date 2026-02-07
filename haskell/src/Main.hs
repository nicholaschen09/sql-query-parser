{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (FromJSON(..), eitherDecode, encode, object, (.=), (.:), withObject)
import Data.Text (Text)
import qualified Data.Text as T
import Web.Scotty
import System.Environment (lookupEnv)
import Network.Wai.Middleware.AddHeaders (addHeaders)

import Parser.Types (Row, QueryResult(..))
import Parser.Core (mkParser)
import qualified Parser.Core as P

data ExecuteRequest = ExecuteRequest
  { erQuery :: Text
  , erData  :: [Row]
  }

instance FromJSON ExecuteRequest where
  parseJSON = withObject "ExecuteRequest" $ \v ->
    ExecuteRequest <$> v .: "query" <*> v .: "data"

main :: IO ()
main = do
  portStr <- lookupEnv "PORT"
  let port = maybe 8082 read portStr :: Int
  putStrLn $ "Haskell SQL Parser server starting on port " ++ show port

  scotty port $ do
    -- CORS headers on all responses
    middleware $ addHeaders
      [ ("Access-Control-Allow-Origin", "*")
      , ("Access-Control-Allow-Methods", "POST, GET, OPTIONS")
      , ("Access-Control-Allow-Headers", "Content-Type")
      ]

    -- OPTIONS preflight
    options (regex ".*") $ do
      text ""

    -- Health check
    get "/health" $ do
      setHeader "Content-Type" "application/json"
      raw $ encode $ object ["status" .= ("ok" :: Text)]

    -- Execute query
    post "/execute" $ do
      body' <- body
      case eitherDecode body' of
        Left err -> do
          setHeader "Content-Type" "application/json"
          raw $ encode $ QueryResult False Nothing (Just $ T.pack $ "Invalid request: " ++ err)
        Right req -> do
          let parser = mkParser (erData req)
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
