module View exposing (view)


import Html exposing
    ( Html
    , div
    , text
    )
import Html.Attributes exposing (class)


import Flags exposing (Flags)
import Mode exposing (Mode(..))
import Model exposing (Model, ViewMode(..))
import Msg exposing (Msg)
import ViewAbilities exposing (viewAbilities)
import ViewDialog exposing (viewDialog)
import ViewToolbar exposing (viewToolbar)
import ViewWorkspace exposing (viewWorkspace)
import WorldParser exposing (parseErrToString)


view : Model -> Html Msg
view model =
    div
        [ class
            ( "level-editor-main"
            ++
                if model.uiState.viewMode == FullScreen then
                    " fullscreen"
                else
                    ""
            )
        ]
        ( case model.flags.mode of
            Edit ->
                (
                    [ viewToolbar model
                    , viewWorkspace model.flags model.uiState model.world
                    ] ++ (viewDialog model)
                )
            View ->
                [ viewAbilities model
                , viewWorkspace model.flags model.uiState model.world ]
        )
