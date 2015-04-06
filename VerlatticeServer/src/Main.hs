{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Control.Monad.IO.Class

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (writeBS "hello world 2") <|>
    route [ ("foo", writeBS "bar")
          , ("fireMissiles", missileHandler)
          , ("echo/:echoparam", echoHandler)
          ] <|>
    dir "static" (serveDirectory ".")

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param

missileHandler :: Snap ()
missileHandler = do
  liftIO fireTheMissiles
  writeBS "Missiles fired."

fireTheMissiles :: IO ()
fireTheMissiles = do
  putStrLn "Firing."
