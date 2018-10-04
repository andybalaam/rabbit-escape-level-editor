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
import Model exposing (Model, UiMode(..), UiState)
import RabbitImage exposing (rabbitImage)
import ThingImage exposing (thingImage)
import World exposing (Block(..), BlockMaterial(..), BlockShape(..))


type ButtonDef =
      CodeButton
    | SaveAndQuitButton
    | UndoButton
    | RedoButton
    | BlockButton
    | ThingButton
    | RabbitButton
    | DeleteButton
    | DetailsButton


buttonsList : List ButtonDef
buttonsList =
    [ SaveAndQuitButton
    , UndoButton
    , RedoButton
    , BlockButton
    , ThingButton
    , RabbitButton
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
        BlockButton ->
            case uiState.mode of
                ChooseBlockMode -> ChangeMode PlaceBlockMode
                _         -> ChangeMode ChooseBlockMode
        ThingButton ->
            case uiState.mode of
                ChooseThingMode -> ChangeMode PlaceThingMode
                _         -> ChangeMode ChooseThingMode
        RabbitButton ->
            case uiState.mode of
                ChooseRabbitMode -> ChangeMode PlaceRabbitMode
                _          -> ChangeMode ChooseRabbitMode
        DeleteButton ->
            ChangeMode DeleteMode
        DetailsButton ->
            case uiState.mode of
                ModifyDetailsMode -> ChangeMode InitialMode
                _           -> ChangeMode ModifyDetailsMode


buttonImage : UiState -> ButtonDef -> String
buttonImage uiState buttondef =
    case buttondef of
        CodeButton -> "code.svg"
        SaveAndQuitButton -> "save.svg"
        UndoButton -> "undo.svg"
        RedoButton -> "redo.svg"
        DetailsButton -> "details.svg"
        BlockButton ->
            case uiState.block of
                Nothing    -> "allblocks.png"
                Just block -> blockImage block
        ThingButton ->
            case uiState.thing of
                Nothing    -> "allthings.png"
                Just thing -> thingImage thing
        RabbitButton ->
            rabbitImage uiState.rabbit
        DeleteButton -> "delete.svg"


pressedClass : UiMode -> ButtonDef -> List (Html.Attribute Msg)
pressedClass mode buttondef =
    let
        pressedTypes =
            case mode of
                InitialMode -> []
                CodeMode _ -> [CodeButton]
                ChooseBlockMode -> [BlockButton]
                PlaceBlockMode -> [BlockButton]
                ChooseThingMode -> [ThingButton]
                PlaceThingMode -> [ThingButton]
                ChooseRabbitMode -> [RabbitButton]
                PlaceRabbitMode -> [RabbitButton]
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
