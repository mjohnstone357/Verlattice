
module Kernel where

-- Generic kernel stuff

-- This simulates getting a value over the network from another instance
makeHandler :: (Read a, Read b, Show a, Show b) => (a -> b) -> a -> IO b
makeHandler f x = do
  putStrLn $ "Got " ++ (show x)
  let remoteRetVal = (f x)
  let serialisedRetVal = show remoteRetVal
  putStrLn $ "The remote client returned " ++ (show serialisedRetVal)
  return $ read serialisedRetVal


-- Implementation-specific stuff

data DoublingRequest = DoublingRequest Int deriving (Read, Show)

doubleNumber :: DoublingRequest -> Int
doubleNumber (DoublingRequest x) = x * 2


-- Combining the Kernel and implementation...

handleNumberDoubling :: DoublingRequest -> IO Int
handleNumberDoubling = makeHandler doubleNumber
