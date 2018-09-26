module ViewWorkspace exposing (viewWorkspace)

import Html exposing (Html, button, div)
import Html.Attributes exposing (alt, class, id, src, style)
import Html.Events exposing (onClick)

import ImagePath exposing (imagePath)
import Flags exposing (Flags)
import Mode exposing (Mode(..))
import Model exposing (UiState, ViewMode(..))
import Msg exposing (Msg(..))
import ViewWorld exposing (viewWorld)
import World exposing (World)



fullScreenButton : UiState-> Flags -> Html Msg
fullScreenButton uiState flags =
    button
        [ onClick (ToggleFullScreen)
        , class "toggle-fullscreen"
        , style
            "background-image"
            ( "url('" ++
                ( imagePath
                    flags
                    ( case uiState.viewMode of
                        FullScreen -> "restore_screen.svg"
                        Normal -> "full_screen.svg"
                    )
                )
                ++ "')"
            )
        , style "background-size" "contain"
        ]
        []


viewWorkspace : Flags -> UiState -> World -> Html Msg
viewWorkspace flags uiState world =
    div
        (
            [ id
                ( case flags.mode of
                    Edit -> "edit-workspace"
                    View -> "view-workspace"
                )
            ]
            ++ (
                if uiState.viewMode == FullScreen then
                    [ class "fullscreen" ]
                else
                    []
            )
        )
        (
            ( case flags.mode of
                Edit -> []
                View -> [ fullScreenButton uiState flags ]
            )
            ++ [ viewWorld flags world ]
        )
