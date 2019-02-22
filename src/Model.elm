module Model exposing (Item(..), Model, UiMode(..), UiState, ViewMode(..))


import Flags exposing (Flags)
import MetaDiff
import MetaLines
import ParseErr exposing (ParseErr(..))
import Rabbit exposing (Rabbit)
import Thing exposing (Thing)
import World exposing (Block(..), World)


type UiMode =
      InitialMode
    | CodeMode String
    | ChooseItemMode
    | PlaceItemMode
    | DeleteMode
    | ModifyDetailsMode


type ViewMode =
      Normal
    | FullScreen


type Item =
       BlockItem Block
     | ThingItem Thing
     | RabbitItem Rabbit


type alias UiState =
    { mode : UiMode
    , viewMode : ViewMode
    , item : Maybe Item
    , newMetaLines : MetaDiff.Diff
    , newWorld : Maybe (String, Result ParseErr World)
    }


type alias Model =
    { flags : Flags
    , world : Result (ParseErr, String) World
    , uiState : UiState
    , t : String -> String
    , past : List World
    , future: List World
    }
