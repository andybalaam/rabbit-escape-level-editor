import Browser
import Html exposing (Html)




import MetaLines
import Model exposing (Model, UiMode(..), UiState)
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


initWorld : String -> World
initWorld initialWorldText =
    let
        p =
            WorldParser.parse "" initialWorldText
    in
        case p of
            Ok w ->
                w
            Err s ->
                makeWorld
                    "Failed to parse provided level"
                    (makeBlockGrid [])
                    []
                    []
                    MetaLines.defaults


translationPlaceholder : String -> String
translationPlaceholder x =
    x


initModel : String -> Model
initModel initialWorldText =
    { world = initWorld initialWorldText
    , uiState =
        { mode = InitialMode
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


type alias Flags = String

init : Flags -> ( Model, Cmd msg )
init flags =
    (initModel flags, Cmd.none)


main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = \(model) -> Sub.none
    }
