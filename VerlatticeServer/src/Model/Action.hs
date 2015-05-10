
module Model.Action where

import Model.Resource

data Action = Action {
  identifier :: ActionID,
  name :: String,
  inputsAndOutputs :: [ActionIO]
} deriving (Eq, Read, Show, Ord)

data ActionIO = ActionIO {
  resource :: ResourceID,
  quantity :: Int
} deriving (Eq, Read, Show, Ord)

-- Later, this will be expanded to allow 'holding' of resources without consuming them
data IOType = Input | Output

data ActionID = ActionID Int
              deriving (Eq, Read, Show, Ord)
