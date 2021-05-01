{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

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
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Except
import Network.Wai (remoteHost)
import Text.Regex
import SpockRes

pastesPerHour = 100
uuidRegex = mkRegex "^[0-9a-f]{8}-?[0-9a-f]{4}-?[0-9a-f]{4}-?[0-9a-f]{4}-?[0-9a-f]{12}$"

app :: Server ()
app = do
    conn <- liftIO $ checkedConnect defaultConnectInfo

    S.get root $ html "<h1>Batbin API</h1>"

    S.post "paste" $ do
        res <- runSpockResT $ do
            pasteContent <- lift $ param' "content"
            host <- lift $ remoteHost <$> request
            ip <- liftIO $ B.fromString <$> sockAddrToIP host

            eReqs <- liftIO (runRedis conn $ R.get ip) >>= getEitherBody

            case eReqs of
                Nothing -> liftIO $ runRedis conn $ R.setex ip 3600 (B.fromString "1") >> pure ()
                Just reqs -> do
                    if (read $ BS.unpack reqs :: Integer) >= pastesPerHour then do
                        ttl <- liftIO (runRedis conn $ R.ttl ip) >>= getEitherBody
                        fail $ "Limit exceeded! Next paste can be stored in " <> show ttl <> "seconds"
                    else do
                        eIncr <- liftIO (runRedis conn $ R.incr ip)  >>= getEitherBody
                        pure ()

            if T.length pasteContent > 50000 then
                fail "Paste too large!"
            else if T.length pasteContent == 0 then
                fail "Paste cannot be empty!"
            else
                do
                    uuid <- liftIO getAvailableUuid
                    pdir <- liftIO pastesDir
                    liftIO $ P.writeFile (makePath pdir uuid) (T.unpack pasteContent)
                    pure uuid
        runBodyRes res

    S.get ("paste" <//> var) $ \id -> do
        res <- runSpockResT $ do
            case matchRegex uuidRegex id of
                Nothing -> fail "Improper paste ID provided!"
                _ -> do
                    pdir <- liftIO pastesDir
                    filePath <- liftIO $ absolutize $ makePath pdir id
                    if pdir `Data.List.isPrefixOf` filePath then do
                        doesExist <- liftIO $ doesFileExist filePath
                        if doesExist then do
                            do 
                                paste <- liftIO $ P.readFile filePath
                                lift $ text $ T.pack paste
                        else fail "This paste does not exist!" 
                    else fail "Can't access pastes outside the paste dir!"
        runBodyRes' res
