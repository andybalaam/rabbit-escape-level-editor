module Flags exposing (Flags)


import Mode exposing (Mode)


type alias Flags =
    { worldText : String
    , mode : Mode
    , urlPrefix : String
    }
