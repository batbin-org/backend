{-# LANGUAGE OverloadedStrings #-}

module Routes where

import Utils
import Web.Spock as S
import Prelude as P
import Control.Monad.IO.Class
import System.Directory
import Data.List (isPrefixOf)
import Data.Text as T
import Database.Redis as R
import Data.ByteString.UTF8 as B
import Data.ByteString.Char8 as BS
import Control.Monad.Except
import Network.Wai (remoteHost)
import Text.Regex

somethingWrong = "Something went wrong!"
pastesPerHour = 80
uuidRegex = mkRegex "^[0-9a-f]{8}-?[0-9a-f]{4}-?[0-9a-f]{4}-?[0-9a-f]{4}-?[0-9a-f]{12}$"

app :: Server ()
app = do
    conn <- liftIO $ checkedConnect defaultConnectInfo

    S.get root $ html "<h1>Batbin API</h1>"

    S.post "paste" $ do
        pasteContent <- param' "content"

        host <- remoteHost <$> request
        ip <- liftIO $ B.fromString <$> sockAddrToIP host

        eReqs <- liftIO $ runRedis conn $ R.get ip

        case eReqs of
            Left _ -> S.json $ getResponse False somethingWrong
            Right Nothing -> liftIO $ runRedis conn $ R.setex ip 3600 (B.fromString "1") >> pure ()
            Right (Just reqs) -> do
                if (read $ BS.unpack reqs :: Integer) >= pastesPerHour then do
                    eTtl <- liftIO $ runRedis conn $ R.ttl ip
                    case eTtl of
                        Left _ -> S.json $ getResponse False somethingWrong
                        Right ttl -> S.json $ getResponse False $ "Limit exceeded! Next paste can be stored in " <> show ttl <> "seconds"
                else do
                    eIncr <- liftIO $ runRedis conn $ R.incr ip
                    case eIncr of
                        Left _ -> S.json $ getResponse False somethingWrong
                        Right _ -> pure ()


        if T.length pasteContent > 50000 then
            S.json $ getResponse False "Paste too large!"
        else if T.length pasteContent == 0 then
            S.json $ getResponse False "Paste cannot be empty!"
        else
            do
                uuid <- liftIO getAvailableUuid
                pdir <- liftIO pastesDir
                liftIO $ P.writeFile (makePath pdir uuid) (T.unpack pasteContent)
                S.json $ getResponse True uuid

    S.get ("paste" <//> var) $ \id -> do
        case matchRegex uuidRegex id of
            Nothing -> S.json $ getResponse False "Improper paste ID provided!"
            _ -> do
                pdir <- liftIO pastesDir
                filePath <- liftIO $ absolutize $ makePath pdir id
                if pdir `Data.List.isPrefixOf` filePath then
                    do
                        doesExist <- liftIO $ doesFileExist filePath
                        if doesExist then
                            do
                                output <- liftIO $ P.readFile filePath
                                text $ T.pack output
                        else
                            do
                                S.json $ getResponse False "This paste does not exist!" 
                else
                    do
                        S.json $ getResponse False "Can't access pastes outside the paste dir!"
