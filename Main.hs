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
import Data.Maybe (fromJust)
import System.Path.NameManip (guess_dotdot, absolute_path)
import Data.List (isPrefixOf)

data Response = Response
  { status :: String
  , message :: String
  }
  deriving (Generic, ToJSON, FromJSON)

type Server a = SpockM () () () a

getResponse :: Bool -> String -> Response
getResponse sts msg = Response { status = if sts then "success" else "failure", message = msg }

-- always relative to project root
pastesDir :: IO String
pastesDir = absolutize "pastes"

getAvailableUuid :: IO String 
getAvailableUuid = do
    uuid <- nextRandom
    pdir <- liftIO pastesDir
    doesExist <- doesFileExist (pdir ++ "/" ++ show uuid)
    if doesExist then getAvailableUuid else return $ T.unpack $ replace "-" "" $ T.pack $ show uuid

makePath :: String -> String -> String
makePath x y = x ++ "/" ++ y

absolutize :: String -> IO String
absolutize aPath = do
        pathMaybeWithDots <- absolute_path aPath
        return $ fromJust $ guess_dotdot pathMaybeWithDots

app :: Server ()
app = do
    get root $ html "<h1>Batbin API</h1>"
    post "/set" $ do
        pasteContent <- param' "content"
        if T.length pasteContent > 12000
             then S.json $ getResponse False "Paste too large!"
             else do
                uuid <- liftIO getAvailableUuid
                pdir <- liftIO pastesDir
                let fileName = replace "-" "" $ T.pack uuid
                liftIO $ writeFile (makePath pdir uuid) (T.unpack pasteContent)
                S.json $ getResponse True uuid
    get "/get" $ do
        id <- param' "id"
        do
            pdir <- liftIO pastesDir
            filePath <- liftIO $ absolutize $ makePath pdir id
            if pdir `Data.List.isPrefixOf` filePath then
                do
                    liftIO $ putStrLn filePath
                    doesExist <- liftIO $ doesFileExist filePath
                    if doesExist then
                        do
                            output <- liftIO $ readFile filePath
                            text $ T.pack output
                    else
                        do
                            S.json $ getResponse False "This paste does not exist!" 
            else
                do
                    S.json $ getResponse False "Can't access pastes outside the paste dir!"

main :: IO()
main = do
    cfg <- defaultSpockCfg () PCNoDatabase ()
    pdir <- liftIO pastesDir
    setSGR [SetColor Foreground Vivid Green]
    putStrLn $ "pastes will be saved to the " ++ pdir ++ " folder"
    createDirectoryIfMissing False pdir
    runSpock 3849 (spock cfg app)
