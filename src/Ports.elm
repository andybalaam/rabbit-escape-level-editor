port module Ports exposing (saveAndQuit)

import Json.Encode as E

port saveAndQuit : E.Value -> Cmd msg
