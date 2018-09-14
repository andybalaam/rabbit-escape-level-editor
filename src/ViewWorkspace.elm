module ViewWorkspace exposing (viewWorkspace)

import Html exposing (Html, div)
import Html.Attributes exposing (id)

import Flags exposing (Flags)
import Msg exposing (Msg)
import ViewWorld exposing (viewWorld)
import World exposing (World)


viewWorkspace : Flags -> World -> Html Msg
viewWorkspace flags world =
    div
        [ id "workspace" ]
        [ viewWorld flags world ]
