module View exposing (view)


import Html exposing
    ( Html
    , div
    )
import Html.Attributes exposing (class)


import Flags exposing (Flags)
import Mode exposing (Mode(..))
import Model exposing (Model)
import Msg exposing (Msg)
import ViewDialog exposing (viewDialog)
import ViewToolbar exposing (viewToolbar)
import ViewWorkspace exposing (viewWorkspace)


view : Model -> Html Msg
view model =
    div
        [ class "level-editor-main"
        ]
        ( case model.flags.mode of
            Edit ->
                (
                    [ (viewToolbar model)
                    , (viewWorkspace model.flags model.world)
                    ] ++ (viewDialog model)
                )
            View ->
                [ viewWorkspace model.flags model.world ]
        )
