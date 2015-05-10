{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Control.Monad.IO.Class
import           Control.Concurrent.MVar
import           Text.JSON
import           Data.Set(elems)
import qualified Data.ByteString.Char8 as BS

import Verlattice

import Model.Resource
import Model.Action
import Model.Plan
import Model.State

main :: IO ()
main = do
  putStrLn "Intialising Verlattice."
  state <- newMVar emptyState
  quickHttpServe (site state)

site :: MVar State -> Snap ()
site stateVar =
  ifTop (method GET (serveFile "/home/matt/Verlattice/VerlatticeClient/index.html")) <|>
  route [ ("scala-js-tutorial-fastopt.js", method GET (serveFile "/home/matt/Verlattice/VerlatticeClient/target/scala-2.11/scala-js-tutorial-fastopt.js"))
        , ("scala-js-tutorial-fastopt.js.map", method GET (serveFile "/home/matt/Verlattice/VerlatticeClient/target/scala-2.11/scala-js-tutorial-fastopt.js.map"))
        , ("foo", writeBS "bar")
        , ("fireMissiles", missileHandler)
        , ("echo/:echoparam", echoHandler)
        , ("version", method GET getVersionHandler)
        ] <|>
  dir "src" (serveDirectory "/home/matt/VerlatticeClient/src/")

getVersionHandler :: Snap ()
getVersionHandler = do
  let versionJSON = toJSObject [("version", (toJSString "0.0.4"))]
  modifyResponse $ setContentType "application/json"
  writeBS $ BS.concat [BS.pack (encode versionJSON), "\n"]

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
