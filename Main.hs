{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Web.Spock as S
import Web.Spock.Config
import Data.Text as T
import Data.Aeson
import GHC.Generics
import Control.Monad.IO.Class
import Data.UUID.V4
import System.Directory
import System.Console.ANSI

data Response = Response
  { status :: String
  , message :: String
  }
  deriving (Generic, ToJSON, FromJSON)

type Server a = SpockM () () () a

getResponse :: Bool -> String -> Response
getResponse sts msg = Response { status = if sts then "success" else "failure", message = msg }

-- always relative to project root
pastesDir :: String
pastesDir = "pastes"

getAvailableUuid :: IO String 
getAvailableUuid = do
    uuid <- nextRandom
    doesExist <- doesFileExist (pastesDir ++ "/" ++ show uuid)
    if doesExist then getAvailableUuid else return $ show uuid

makePath :: String -> String -> String
makePath x y = x ++ "/" ++ y

app :: Server ()
app = do
    get root $ do
        html "<h1>Batbin API</h1>"
    post "/set" $ do
        pasteContent <- param' "content"
        if T.length pasteContent > 12000
             then S.json $ getResponse False "Paste too large!"
             else do
                uuid <- liftIO getAvailableUuid
                liftIO $ writeFile (makePath pastesDir uuid) (T.unpack pasteContent)
                S.json $ getResponse True uuid
    get "/get" $ do
        id <- param' "id"
        do
            let filePath = makePath pastesDir id
            doesExist <- liftIO $ doesFileExist filePath
            if doesExist then
                do 
                    output <- liftIO $ readFile filePath
                    text $ T.pack output
            else
                S.json $ getResponse False "This paste does not exist!"

main :: IO()
main = do
    cfg <- defaultSpockCfg () PCNoDatabase ()
    currentDir <- getCurrentDirectory
    setSGR [SetColor Foreground Vivid Green]
    putStrLn $ "pastes will be saved to the " ++ makePath currentDir pastesDir ++ " folder"
    createDirectoryIfMissing False pastesDir
    runSpock 3000 (spock cfg app)
