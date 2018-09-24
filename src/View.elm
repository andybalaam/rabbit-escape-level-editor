module View exposing (view)


import Html exposing
    ( Html
    , div
    , text
    )
import Html.Attributes exposing (class)


import Flags exposing (Flags)
import Mode exposing (Mode(..))
import Model exposing (Model)
import Msg exposing (Msg)
import ViewDialog exposing (viewDialog)
import ViewToolbar exposing (viewToolbar)
import ViewWorkspace exposing (viewWorkspace)
import WorldParser exposing (parseErrToString)


view : Model -> Html Msg
view model =
    div
        [ class "level-editor-main"
        ]
        ( case model.world of
            Ok w ->
                ( case model.flags.mode of
                    Edit ->
                        (
                            [ (viewToolbar model)
                            , (viewWorkspace model.flags w)
                            ] ++ (viewDialog model w)
                        )
                    View ->
                        [ viewWorkspace model.flags w ]
                )
            Err e ->
                [
                    ( div
                        [ class "view-parse-err" ]
                        [ text (parseErrToString e) ]
                    )
                ]
        )
