module ViewDialog exposing (viewDialog)


import Html exposing (Html, button, div, img, input, label, p, text, textarea)
import Html.Attributes exposing
    ( class
    , disabled
    , for
    , id
    , readonly
    , spellcheck
    , src
    , style
    , type_
    , value
    )
import Html.Events exposing (onClick, onInput)


import BlockImage exposing (blockImage)
import ImagePath exposing (imagePath)
import MetaLines exposing (MetaValue(..))
import Model exposing (Item(..), Model, UiMode(..), UiState)
import Msg exposing (Msg(..))
import World exposing (Block(..), BlockMaterial(..), BlockShape(..), World)
import WorldParser exposing (parseErrToString)
import Rabbit exposing
    ( Direction(..)
    , Rabbit
    , RabbitType(..)
    , makeRabbit
    , makeRabbot
    )
import RabbitImage exposing (rabbitImage)
import Thing exposing (Thing(..), TokenType(..))
import ThingImage exposing (thingImage)


type alias Contents =
    { visible : Bool
    , dialogStyles : List (Html.Attribute Msg)
    , items : List (Html Msg)
    }


-- Make a string into a paragraph of translated text
tp : Model -> String -> List (Html.Attribute Msg) -> Html Msg
tp model s attrs =
    p attrs [text (model.t s)]


blockButtons : Model -> List (Html Msg)
blockButtons model =
    let
        but : Block -> Html Msg
        but block =
            button
                [ onClick (ChangeItem (BlockItem block)) ]
                [ img [ src (imagePath model.flags (blockImage block)) ] [] ]
    in
        [ tp model "Choose a block:" []
        , but (Block Earth Flat)
        , but (Block Earth UpRight)
        , but (Block Earth UpLeft)
        , but (Block Earth BridgeUpRight)
        , but (Block Earth BridgeUpLeft)
        , but (Block Metal Flat)
        ]


rabbitButtons : Model -> List (Html Msg)
rabbitButtons model =
    let
        but : Rabbit -> Html Msg
        but rabbit =
            button
                [ onClick (ChangeItem (RabbitItem rabbit)) ]
                [ img [ src (imagePath model.flags (rabbitImage rabbit)) ] [] ]
    in
        [ tp model "Choose a rabbit:" []
        , but (makeRabbit 0 0 Right)
        , but (makeRabbit 0 0 Left)
        , but (makeRabbot 0 0 Right)
        , but (makeRabbot 0 0 Left)
        ]


thingButtons : Model -> List (Html Msg)
thingButtons model =
    let
        but : Thing -> Html Msg
        but thing =
            button
                [ onClick (ChangeItem (ThingItem thing)) ]
                [ img [ src (imagePath model.flags (thingImage thing)) ] [] ]
    in
        [ tp model "Choose an item:" []
        , but (Entrance 0 0)
        , but (Exit 0 0)
        , but (Fire 0 0)
        , but (Token Bash 0 0)
        , but (Token Dig 0 0)
        , but (Token Bridge 0 0)
        , but (Token BlockT 0 0)
        , but (Token Climb 0 0)
        , but (Token Explode 0 0)
        , but (Token Brolly 0 0)
        ]


chooseItemButtons : Model -> Contents
chooseItemButtons model =
    { visible =
        True
    , dialogStyles =
        [ style "overflow" "auto" ]
    , items =
        (  blockButtons model
        ++ thingButtons model
        ++ rabbitButtons model
        )
    }


codeText : Model -> World -> String -> Contents
codeText model world initialCode =
    let
        (code, parsed) =
            case model.uiState.newWorld of
                Nothing -> (initialCode, Ok world)
                Just x -> x

        (parseError, canUpdate) =
            case parsed of
                Ok _ -> ("", True)
                Err e -> (parseErrToString e, False)

    in
        { visible =
            True
        , dialogStyles =
            [ style "display" "grid"
            , style "grid-template-rows" "3em 3em 3fr 1fr"
            ]
        , items =
            [ tp model
                (  "Copy this code and paste it somewhere to save. Paste it into "
                ++ "Rabbit Escape to play it."
                )
                []
            , p
                []
                [ button
                    [ class "dialogSubmit"
                    , onClick (ChangeMode InitialMode) -- TODO
                    ]
                    [ text "Cancel" ]
                , button
                    ( [ class "dialogSubmit"
                        , onClick ChangeCode
                      ]
                    ++ if canUpdate then [] else [ disabled True ]
                    )
                    [ text "Update" ]
                ]
            , textarea
                [ id "code"
                , onInput CodeInput
                , spellcheck False
                ]
                [ text code
                ]
            , textarea
                [ id "errors"
                , readonly True
                , spellcheck False
                ]
                [ text parseError ]
            ]
        }


metaLineBoxes
    : MetaLines.Diff
    -> (String, String)
    -> List (Html Msg)
metaLineBoxes diff (name, defVal) =
    let
        inpid : String
        inpid = "id" ++ name

        diffVal : Maybe MetaLines.DiffValue
        diffVal = MetaLines.getDiff name diff

        val : String
        val = Maybe.withDefault defVal (Maybe.map .raw diffVal)

        lbl : {text : String, attrs : List (Html.Attribute Msg)}
        lbl =
            case diffVal of
                Just v ->
                    case v.parsed of
                        Err _ ->  -- Will need more if not just ints
                            {text="Should be a number!", attrs=[class "error"]}
                        Ok _ ->
                            {text=name, attrs=[]}
                _ ->
                    {text=name, attrs=[]}
    in
        [ label ( [ for inpid ] ++ lbl.attrs ) [ text lbl.text ]
        , input
            [ id inpid
            , type_ "text"
            , value val
            , onInput (DetailsInput name)
            ]
            []
        ]


modifyDetailsControls : Model -> World-> Contents
modifyDetailsControls model world =
    let
        canUpdate : Bool
        canUpdate = MetaLines.allOk model.uiState.newMetaLines

        boxes : List (List (Html Msg))
        boxes =
            List.map
                (metaLineBoxes model.uiState.newMetaLines)
                (MetaLines.toStringList world.metaLines)
    in
        { visible =
            True
        , dialogStyles =
            [ style "overflow" "auto"
            , style "display" "grid"
            , style "grid-template-columns" "1fr 2fr"
            , style "grid-template-rows" "2em 3em"
            , style "grid-auto-rows" "2em"
            ]
        , items =
            ( [ tp model
                (  "Change the values and choose 'Update'."
                )
                [ style "grid-column-start" "1"
                , style "grid-column-end" "3"
                ]
            , p
                [ style "grid-column-start" "1"
                , style "grid-column-end" "3"
                ]
                [ button
                    [ class "dialogSubmit"
                    , onClick (ChangeMode InitialMode)
                    ]
                    [ text "Cancel" ]
                , button
                    ( [ class "dialogSubmit"
                      , onClick ChangeDetails
                      ]
                    ++ if canUpdate then [] else [ disabled True ]
                    )
                    [ text "Update" ]
                ]
            ]
            ++ List.concat boxes
            )
        }


invisible : Contents
invisible =
    { visible = False
    , dialogStyles = []
    , items = []
    }


drawDialog : Contents -> List (Html Msg)
drawDialog contents =
    let
        vs =
            if contents.visible then
                [style "visibility" "visible"]
            else
                []
    in
        [ div
            ( [ id "dialogBackground" ] ++ vs )
            []
        , div
            ( [ id "dialog" ] ++ vs ++ contents.dialogStyles )
            contents.items
        ]


viewDialog : Model -> World -> List (Html Msg)
viewDialog model world =
    drawDialog
        ( case model.uiState.mode of
            ChooseItemMode    -> chooseItemButtons model
            CodeMode code     -> codeText model world code
            ModifyDetailsMode -> modifyDetailsControls model world
            other             -> invisible
        )
