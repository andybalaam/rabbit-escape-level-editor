import Browser
import Html exposing (Html)
import Json.Decode


import Flags exposing (Flags)
import FlagsDecoder exposing (flagsDecoder)
import MetaLines
import Mode exposing (Mode(..))
import Model exposing (Model, UiMode(..), UiState, ViewMode(..))
import Msg exposing (Msg(..))
import ParseErr exposing (ParseErr)
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
import WorldParser exposing (parse, stringRemoveLastIfEmpty)
import WorldTextRender exposing (render)


translationPlaceholder : String -> String
translationPlaceholder x =
    x


initModel : Flags -> String -> Model
initModel flags initialWorldText =
    case WorldParser.parse "" initialWorldText of
        Ok w ->
            initModelRecord
                flags
                (Ok w)
                InitialMode
        Err e ->
            let
                badWorldText = (stringRemoveLastIfEmpty initialWorldText)
            in
                initModelRecord
                    flags
                    (Err (e, badWorldText))
                    (CodeMode badWorldText)


initModelRecord : Flags -> Result (ParseErr, String) World -> UiMode -> Model
initModelRecord flags world mode =
    { flags = flags
    , world = world
    , uiState =
        { mode = mode
        , viewMode = Normal
        , item = Nothing
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
