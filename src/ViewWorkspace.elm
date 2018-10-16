module ViewWorkspace exposing (viewWorkspace)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (alt, class, id, src, style)
import Html.Events exposing (onClick)

import ImagePath exposing (imagePath)
import Flags exposing (Flags)
import Mode exposing (Mode(..))
import Model exposing (UiState, ViewMode(..))
import Msg exposing (Msg(..))
import ParseErr exposing (ParseErr)
import ViewWorld exposing (viewWorld)
import World exposing (World)
import WorldParser exposing (parseErrToString)



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


viewWorkspace : Flags -> UiState -> Result (ParseErr, String) World -> Html Msg
viewWorkspace flags uiState world =
    case world of
        Ok w -> viewWorkspaceWorld flags uiState w
        Err (e, _) -> viewWorkspaceError (parseErrToString e)


viewWorkspaceError : String -> Html Msg
viewWorkspaceError errString =
    div
        [ class "view-parse-err" ]
        [ text errString ]


viewWorkspaceWorld : Flags -> UiState -> World -> Html Msg
viewWorkspaceWorld flags uiState world =
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
