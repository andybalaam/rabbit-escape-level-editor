module Msg exposing (Msg(..))


import Model exposing (Item, UiMode)
import Rabbit exposing (Rabbit)
import Thing exposing (Thing)
import World exposing (Block)


type Msg =
      LevelClick Int Int
    | ChangeMode UiMode
    | ChangeItem Item
    | AddColumn
    | RemoveColumn
    | AddRow
    | RemoveRow
    | Undo
    | Redo
    | DetailsInput String String
    | ChangeDetails
    | CodeInput String
    | ChangeCode
    | ToggleFullScreen
    | SaveAndQuit
