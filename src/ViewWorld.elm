module ViewWorld exposing (viewWorld)

import Html exposing (Html, button, div, img)
import Html.Attributes exposing (id, class, src, style)
import Html.Events exposing (onClick)


import BlockImage exposing (blockImage)
import Flags exposing (Flags)
import ImagePath exposing (imagePath)
import Mode exposing (Mode(..))
import Model exposing (Model)
import Msg exposing (Msg(..))
import Rabbit exposing (Direction(..), Rabbit)
import RabbitImage exposing (rabbitImage)
import Thing exposing (Thing)
import ThingImage exposing (thingImage)
import World exposing
    ( Block(..)
    , BlockMaterial(..)
    , BlockShape(..)
    , World
    , rabbitsAt
    , thingsAt
    )


rabbitImg : Flags -> Rabbit -> Html Msg
rabbitImg flags rabbit =
    img
        [ src (imagePath flags (rabbitImage (Just rabbit)))
        , class "thing"
        ]
        []


thingImg : Flags -> Thing -> Html Msg
thingImg flags thing =
    img
        [ src (imagePath flags (thingImage (Just thing)))
        , class "thing"
        ]
        []


blockImg : Flags -> Block -> Int -> Int -> List (Html Msg)
blockImg flags block x y =
    case block of
        NoBlock -> []
        _ ->
            [ img
                [ src (imagePath flags (blockImage block)) ]
                []
            ]


gridPosAttrs : Int -> Int -> List (Html.Attribute Msg)
gridPosAttrs x y =
    let
        sx = String.fromInt (x + 1)
        sy = String.fromInt (y + 1)
    in
        [ style "grid-row-start" sy
        , style "grid-row-end" sy
        , style "grid-column-start" sx
        , style "grid-column-end" sx
        , id (sx ++ "," ++ sy)
        ]



viewBlockContents :
    Flags ->
    List (Html Msg) ->
    Int ->
    Int ->
    Html Msg
viewBlockContents flags images x y =
    case flags.mode of
        Edit ->
            button
                ( [ onClick (LevelClick x y) ] ++ gridPosAttrs x y )
                images
        View ->
            div (gridPosAttrs x y) images


addCol : Flags -> Int -> Int -> List (Html Msg)
addCol flags x y =
    case y of
        0 ->
            [ button
                ( [ onClick (AddColumn) ] ++ gridPosAttrs x y )
                [ img [ src (imagePath flags "add_column.svg") ] [] ]
            ]
        1 ->
            [ button
                ( [ onClick (RemoveColumn) ] ++ gridPosAttrs x y )
                [ img [ src (imagePath flags "remove_column.svg") ] [] ]
            ]
        _ ->
            []


addRow : Flags -> Int -> List (Html Msg)
addRow flags y =
    [ button
        ( [ onClick (AddRow) ] ++ gridPosAttrs 0 y )
        [ img [ src (imagePath flags "add_row.svg") ] [] ]
    , button
        ( [ onClick (RemoveRow) ] ++ gridPosAttrs 1 y )
        [ img [ src (imagePath flags "remove_row.svg") ] [] ]
    ]


viewBlock : Flags -> World -> Int -> Int -> Block -> Html Msg
viewBlock flags world y x block =
    viewBlockContents
        flags
        (  blockImg flags block x y
        ++ List.map (thingImg flags) (thingsAt world x y)
        ++ List.map (rabbitImg flags) (rabbitsAt world x y)
        )
        x
        y


viewRow : Flags -> World -> Int -> List Block -> List (Html Msg)
viewRow flags world y blocks =
       ( List.indexedMap (viewBlock flags world y) blocks)
    ++ addCol flags (List.length blocks) y


viewWorld : Flags -> World -> Html Msg
viewWorld flags world =
    div
        [ id "level" ]
        (
            ( List.concat
                (List.indexedMap
                    (viewRow flags world)
                    (World.blocks world)
                )
            )
        ++ addRow flags (List.length (World.blocks world))
        )
