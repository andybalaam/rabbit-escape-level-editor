module ViewToolbar exposing (viewToolbar)

import BlockImage exposing (blockImage)
import Html exposing
    ( Html
    , button
    , div
    , img
    )
import Html.Attributes exposing (class, disabled, id, src)
import Html.Events exposing (onClick)

import ImagePath exposing (imagePath)
import Msg exposing (Msg(..))
import Model exposing (Item(..), Model, UiMode(..), UiState)
import RabbitImage exposing (rabbitImage)
import ThingImage exposing (thingImage)
import World exposing (Block(..), BlockMaterial(..), BlockShape(..))


type ButtonDef =
      SaveAndQuitButton
    | UndoButton
    | RedoButton
    | ItemButton
    | DeleteButton
    | DetailsButton
    | CodeButton


buttonsList : List ButtonDef
buttonsList =
    [ SaveAndQuitButton
    , UndoButton
    , RedoButton
    , ItemButton
    , DeleteButton
    , DetailsButton
    , CodeButton
    ]


buildClickCmd : UiState -> ButtonDef -> Msg
buildClickCmd uiState buttonDef =
    case buttonDef of
        CodeButton  ->
            case uiState.mode of
                CodeMode _ -> ChangeMode InitialMode
                _    -> ChangeMode (CodeMode "")
        SaveAndQuitButton ->
            SaveAndQuit
        UndoButton ->
            Undo
        RedoButton ->
            Redo
        ItemButton ->
            case uiState.mode of
                ChooseItemMode -> ChangeMode PlaceItemMode
                _              -> ChangeMode ChooseItemMode
        DeleteButton ->
            ChangeMode DeleteMode
        DetailsButton ->
            case uiState.mode of
                ModifyDetailsMode -> ChangeMode InitialMode
                _                 -> ChangeMode ModifyDetailsMode


buttonImage : UiState -> ButtonDef -> String
buttonImage uiState buttondef =
    case buttondef of
        CodeButton -> "code.svg"
        SaveAndQuitButton -> "save.svg"
        UndoButton -> "undo.svg"
        RedoButton -> "redo.svg"
        DetailsButton -> "details.svg"
        ItemButton ->
            case uiState.item of
                Nothing    -> "allitems.png"
                Just item ->
                    case item of
                        BlockItem b -> blockImage b
                        ThingItem t -> thingImage t
                        RabbitItem r -> rabbitImage r
        DeleteButton -> "delete.svg"


pressedClass : UiMode -> ButtonDef -> List (Html.Attribute Msg)
pressedClass mode buttondef =
    let
        pressedTypes =
            case mode of
                InitialMode -> []
                CodeMode _ -> [CodeButton]
                ChooseItemMode -> [ItemButton]
                PlaceItemMode -> [ItemButton]
                DeleteMode -> [DeleteButton]
                ModifyDetailsMode -> [DetailsButton]
    in
        if List.member buttondef pressedTypes then
            [ class "pressed" ]
        else
            []


buttonEnabled : Model -> ButtonDef -> List (Html.Attribute Msg)
buttonEnabled model buttondef =
    if
        case model.uiState.mode of
            CodeMode _ ->
                case buttondef of
                    CodeButton -> False
                    _ -> True
            ModifyDetailsMode ->
                case buttondef of
                    DetailsButton -> False
                    _ -> True
            _ ->
                case buttondef of
                    UndoButton -> List.isEmpty model.past
                    RedoButton -> List.isEmpty model.future
                    _ -> False
    then
        [disabled True]
    else
        []


viewButton : Model -> ButtonDef -> Html Msg
viewButton model def =
    button
        ( [ onClick (buildClickCmd model.uiState def) ]
          ++ pressedClass model.uiState.mode def
          ++ buttonEnabled model def
        )
        [ img
            [ src (imagePath model.flags (buttonImage model.uiState def)) ]
            []
        ]


viewToolbar : Model -> Html Msg
viewToolbar model =
    div
        [ id "toolbar" ]
        (List.map (viewButton model) buttonsList)
