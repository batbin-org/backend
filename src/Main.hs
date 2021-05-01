{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Spock as S
import Web.Spock.Config
import Control.Monad.IO.Class
import System.Directory
import System.Console.ANSI
import Utils
import Routes

main :: IO()
main = do
    pdir <- liftIO pastesDir
    setSGR [SetColor Foreground Vivid Green]
    putStrLn $ "pastes will be saved to the " ++ pdir ++ " folder"
    setSGR [SetColor Foreground Vivid Yellow]
    cfg <- defaultSpockCfg () PCNoDatabase ()
    createDirectoryIfMissing False pdir
    runSpock 3849 (spock cfg app)