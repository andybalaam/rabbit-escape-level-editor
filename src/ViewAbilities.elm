module ViewAbilities exposing (viewAbilities)

import Dict
import Html exposing
    ( Html
    , button
    , div
    , img
    , text
    )
import Html.Attributes exposing (disabled, id, src)

import ImagePath exposing (imagePath)
import MetaValue exposing (MetaValue(..))
import Model exposing (Model)
import Msg exposing (Msg)
import World exposing (World)
import Rabbit exposing (Rabbit, RabbitType(..))


allAbilities =
    [  "bash"
    ,  "dig"
    ,  "bridge"
    ,  "block"
    ,  "climb"
    ,  "explode"
    ,  "brolly"
    ]


abilityNumber : Model -> String -> Int
abilityNumber model ability =
    case model.world of
        Ok w ->
            case Dict.get ability w.metaLines of
                Just (MvInt i) -> i
                _ -> 0
        _ -> 0


tokenImage : Model -> String -> String
tokenImage model ability =
    (imagePath model.flags ("token_" ++ ability ++ ".svg"))


viewAbility : Model -> String -> Maybe (Html Msg)
viewAbility model ability =
    case abilityNumber model ability of
        0 ->
            Nothing
        n ->
            Just
                ( button
                    [ disabled True ]
                    [ img
                        [ src (tokenImage model ability) ]
                        []
                    , text (String.fromInt n)
                    ]
                )


zeroIfMissing : World -> String -> Int
zeroIfMissing world name =
    case Dict.get name world.metaLines of
        Just (MvInt i) -> i
        _ -> 0


numToSave : World -> Int
numToSave world =
    zeroIfMissing world "num_to_save"


numNormalRabbits : List Rabbit -> Int
numNormalRabbits rabbits =
    List.length
        ( List.filter
            (\r -> r.typ == Normal)
            rabbits
        )


numRabbits : World -> Int
numRabbits world =
    (zeroIfMissing world "num_rabbits") + (numNormalRabbits world.rabbits)


toSaveText : Model -> List (Html Msg)
toSaveText model =
    case model.world of
        Ok w ->
            [
                text
                (  " (Save "
                ++ (String.fromInt (numToSave w))
                ++ "/"
                ++ (String.fromInt (numRabbits w))
                ++ " rabbits)"
                )
           ]
        _ ->
            []


viewAbilities : Model -> Html Msg
viewAbilities model =
    div
        [ id "abilitybar" ]
        (  (List.filterMap (viewAbility model) allAbilities)
        ++ (toSaveText model)
        )

