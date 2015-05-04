
module Services where

{- This will be used to generate the client/server API, including Scala code and JSON serialisation
and deserialisation logic on the client and the server.

The intent is to provide the client with an alternative to making expensive requests.  For example,
the client will need a list of the names of all the actions in order to display them when the user
is selecting an action to add to a plan. At that point, the client doesn't care about the action's
inputs and outputs. so it's a waste to send them down. -}

import Data.Map.Strict
import qualified Data.Map.Strict as Map

data Request = GetActionNamesRequest

data Response = GetActionNamesResponse [String]

type RequestHandler = String -> String

serviceHandlers :: Map String RequestHandler
serviceHandlers = Map.fromList [
  ("getActionNames", getActionNames)
  ]

resolveService :: String -> String -> String
resolveService serviceName requestJSON =
  let (Just handler) = Map.lookup serviceName serviceHandlers in
  handler requestJSON

getActionNames :: String -> String
getActionNames requestJSON =
  let GetActionNamesRequest = parseRequest requestJSON in
  error "nyi"

parseRequest :: String -> Request
parseRequest requestJSON =
  if ("{\"requestType\":\"GetActionNamesRequest\"" `isPrefixOf` requestJSON)
  then GetActionNamesRequest
  else error "unknown request type in request JSON"
