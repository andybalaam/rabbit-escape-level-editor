module FlagsDecoder exposing (flagsDecoder)


import Json.Decode as D


import Flags exposing (Flags)
import Mode exposing (Mode(..))


decodeMode s =
    case s of
        "View" -> View
        "Edit" -> Edit
        _ -> View


modeDecoder : D.Decoder Mode
modeDecoder =
    D.oneOf [ D.map decodeMode D.string ]


defaultWorldText : String
defaultWorldText = "###\n# #\n###\n"


flagsDecoder : D.Decoder Flags
flagsDecoder =
    D.map3 Flags
        (D.oneOf [(D.field "worldText" D.string), D.succeed defaultWorldText])
        (D.oneOf [(D.field "mode" modeDecoder), D.succeed View])
        (D.oneOf [(D.field "urlPrefix" D.string), D.succeed ""])
