{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Utils where

import Web.Spock as S
import Web.Spock.Config
import Data.Text as T
import GHC.Generics
import Data.Aeson
import Data.UUID.V4
import Control.Monad.IO.Class
import System.Directory
import Data.Maybe
import System.Path.NameManip
import Data.IP
import Network.Socket

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

sockAddrToIP :: SockAddr -> IO String
sockAddrToIP sockAddr = case sockAddr of
    SockAddrInet _port hostAddress -> return $ show $ IPv4 $ fromHostAddress hostAddress
    SockAddrInet6 _port _flowInfo hostAddress6 _scopeID -> return $ show $ IPv6 $ fromHostAddress6 hostAddress6
    SockAddrUnix _ -> error "UNIX address found on the web!? What?!"