
module Model.Resource where

data Resource = Resource {
  identifier :: ResourceID,
  name :: String
} deriving (Eq, Read, Show, Ord)

data ResourceID = ResourceID Int
                deriving (Eq, Read, Show, Ord)
