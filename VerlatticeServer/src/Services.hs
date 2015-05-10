
module Services where

{- This will be used to generate the client/server API, including Scala code and JSON serialisation
and deserialisation logic on the client and the server.

The intent is to provide the client with an alternative to making expensive requests.  For example,
the client will need a list of the names of all the actions in order to display them when the user
is selecting an action to add to a plan. At that point, the client doesn't care about the action's
inputs and outputs. so it's a waste to send them down. -}

import Data.Map.Strict
import qualified Data.Map.Strict as Map
import Data.List

import API
import Model.State

type RequestHandler = State -> String -> StateAndResponse

data StateAndResponse = StateAndResponse {
  newState :: State,
  responseJSON :: String
}

serviceHandlers :: Map String RequestHandler
serviceHandlers = Map.fromList [
  ("getActionNames", processGetActionNames)
  ]

-- TODO The caller of this function should update the system state
resolveService :: State -> String -> String -> StateAndResponse
resolveService state serviceName requestJSON =
  let (Just handler) = Map.lookup serviceName serviceHandlers in
  handler state requestJSON

-- We'll map a service URL to a function like this
processGetActionNames :: RequestHandler
processGetActionNames state requestJSON =
  let GetActionNamesRequest = parseRequest requestJSON;
      result = getActionNames state in
  StateAndResponse {
    newState = resultantState result,
    responseJSON = show $ response result
    }


parseRequest :: String -> Request
parseRequest requestJSON =
  if ("{\"requestType\":\"GetActionNamesRequest\"" `isPrefixOf` requestJSON)
  then GetActionNamesRequest
  else error "unknown request type in request JSON"
