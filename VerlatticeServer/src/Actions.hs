
module Actions where

import Text.JSON

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

instance JSON ActionInput where
  showJSON actionInput =
    let name = inputResourceName actionInput :: String;
        quantity = show $ inputQuantity actionInput :: String;
        resourcesJSON = toJSObject [("inputResourceName", JSString (toJSString name)),
                                    ("inputQuantity", JSString (toJSString quantity))] in
    JSObject resourcesJSON

  readJSON (JSObject o) =
    let pairs = (fromJSObject o) :: [(String, JSValue)];
        (Just jsName) = (lookup "inputResourceName" pairs) :: Maybe JSValue;
        (Just jsQuantity) = lookup "inputQuantity" pairs :: Maybe JSValue;
        (JSString jsStringName) = jsName;
        (JSString jsStringQuantity) = jsQuantity
    in
     Ok ActionInput{inputResourceName = fromJSString jsStringName,
                 inputQuantity = (read (fromJSString jsStringQuantity)) :: Int}
  readJSON _ = Error "invalid json type"


exampleActionInput :: ActionInput
exampleActionInput = ActionInput {
  inputResourceName = "foo",
  inputQuantity = 500
}

exampleJSON :: String
exampleJSON = "{\"inputResourceName\":\"foo\",\"inputQuantity\":\"500\"}"
