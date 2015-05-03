
module Actions where

data Action = Action {
  actionName :: String,
  inputs :: [ActionInput],
  outputs :: [ActionOutput]
} deriving (Eq, Read, Show, Ord)

data ActionInput = ActionInput {
  inputResourceName :: String,
  inputQuantity :: Int
} deriving (Eq, Read, Show, Ord)

data ActionOutput = ActionOutput {
  outputResourceName :: String,
  outputQuantity :: Int
} deriving (Eq, Read, Show, Ord)

--instance JSON 

