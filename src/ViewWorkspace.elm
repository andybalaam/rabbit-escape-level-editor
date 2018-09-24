module ViewWorkspace exposing (viewWorkspace)

import Html exposing (Html, div)
import Html.Attributes exposing (class, id)

import Flags exposing (Flags)
import Mode exposing (Mode(..))
import Msg exposing (Msg)
import ViewWorld exposing (viewWorld)
import World exposing (World)


viewWorkspace : Flags -> World -> Html Msg
viewWorkspace flags world =
    div
        [ id
            ( if flags.mode == Edit then
                "edit-workspace"
            else
                "view-workspace"
            )
        ]
        [ viewWorld flags world ]
