
module Model.Resource where

data Resource = Resource {
  identifier :: ResourceID,
  name :: String
}

data ResourceID = ResourceID Int
                deriving (Eq, Read, Show, Ord)

