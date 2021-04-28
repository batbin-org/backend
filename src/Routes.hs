{-# LANGUAGE OverloadedStrings #-}

module Routes where

import Utils
import Web.Spock as S
import Control.Monad.IO.Class
import System.Directory
import Data.List (isPrefixOf)
import Data.Text as T

app :: Server ()
app = do
    get root $ html "<h1>Batbin API</h1>"

    post "/paste" $ do
      pasteContent <- param' "content"
      if T.length pasteContent > 12000 then
         S.json $ getResponse False "Paste too large!"
      else if T.length pasteContent == 0 then
         S.json $ getResponse False "Paste cannot be empty!"
      else
         do
            uuid <- liftIO getAvailableUuid
            pdir <- liftIO pastesDir
            liftIO $ writeFile (makePath pdir uuid) (T.unpack pasteContent)
            S.json $ getResponse True uuid

    get ("/paste" <//> var) $ \id -> do
        do
            pdir <- liftIO pastesDir
            filePath <- liftIO $ absolutize $ makePath pdir id
            if pdir `Data.List.isPrefixOf` filePath then
                do
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