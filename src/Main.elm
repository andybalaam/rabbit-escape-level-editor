import Browser
import Html exposing (Html)
import Json.Decode


import Flags exposing (Flags)
import FlagsDecoder exposing (flagsDecoder)
import MetaLines
import Mode exposing (Mode(..))
import Model exposing (Model, UiMode(..), UiState, ViewMode(..))
import Msg exposing (Msg(..))
import Rabbit exposing (Direction(..), Rabbit, makeRabbit)
import Update exposing (update)
import View exposing (view)
import World exposing
    ( Block(..)
    , BlockMaterial(..)
    , BlockShape(..)
    , World
    , makeBlockGrid
    , makeWorld
    )
import WorldParser exposing (parse)
import WorldTextRender exposing (render)


translationPlaceholder : String -> String
translationPlaceholder x =
    x


initModel : Flags -> String -> Model
initModel flags initialWorldText =
    { flags = flags
    , world = WorldParser.parse "" initialWorldText
    , uiState =
        { mode = InitialMode
        , viewMode = Normal
        , block = Nothing
        , rabbit = Just (makeRabbit 0 0 Right)
        , thing = Nothing
        , newMetaLines = MetaLines.emptyDiff
        , newWorld = Nothing
        }
    , t = translationPlaceholder
    , past = []
    , future = []
    }


defaultFlags : Flags
defaultFlags =
    { worldText = "##\n##\n"
    , mode = View
    , urlPrefix = ""
    }


init : Json.Decode.Value -> ( Model, Cmd msg )
init flagsJson =
    let
        flags : Flags
        flags = Result.withDefault
            defaultFlags
            (Json.Decode.decodeValue flagsDecoder flagsJson)
    in
        (initModel flags flags.worldText, Cmd.none)


main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = \(model) -> Sub.none
    }
