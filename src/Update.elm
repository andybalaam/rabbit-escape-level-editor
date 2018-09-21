module Update exposing (update)


import MetaLines exposing (MetaLines)
import Model exposing (Model, UiMode(..))
import Msg exposing (Msg(..))
import Rabbit exposing (Rabbit, movedRabbit)
import Thing exposing (Thing(..))
import World exposing
    ( Block(..)
    , BlockMaterial(..)
    , BlockShape(..)
    , World
    , blocks
    , makeWorld
    , makeBlockGrid
    )
import WorldParser exposing (parse)
import WorldTextRender


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let

        updatedModel =
            case msg of
                Undo -> updateUndo model
                Redo -> updateRedo model
                _ -> normalUpdate msg model
    in
        (updatedModel, Cmd.none)


normalUpdate : Msg -> Model -> Model
normalUpdate msg model =
    let

        updatedModel =
            case msg of
                LevelClick x y ->
                    case model.uiState.mode of
                        InitialMode ->
                            updateLevelClickBlock model x y
                        PlaceBlockMode ->
                            updateLevelClickBlock model x y
                        PlaceThingMode ->
                            updateLevelClickThing model x y
                        PlaceRabbitMode ->
                            updateLevelClickRabbit model x y
                        _ ->
                            model
                ChangeMode mode ->
                    updateChangeMode model mode
                ChangeBlock block ->
                    updateChangeBlock model block
                ChangeThing thing ->
                    updateChangeThing model thing
                ChangeRabbit rabbit ->
                    updateChangeRabbit model rabbit
                AddColumn ->
                    updateAddColumn model
                RemoveColumn ->
                    updateRemoveColumn model
                AddRow ->
                    updateAddRow model
                RemoveRow ->
                    updateRemoveRow model
                DetailsInput name value ->
                    updateDetailsInput model name value
                ChangeDetails ->
                    updateChangeDetails model
                CodeInput text ->
                    updateCodeInput model text
                ChangeCode ->
                    updateChangeCode model
                Undo ->
                    model  -- Should never happen - covered in update
                Redo ->
                    model  -- Should never happen - covered in update
    in
        -- If something changed, remember in undo stack.
        if updatedModel.world == model.world then
            updatedModel
        else
            case model.world of
                Err _ ->
                    updatedModel
                Ok w ->
                    { updatedModel
                    | past = w :: model.past
                    , future = []
                    }


updateCodeInput : Model -> String -> Model
updateCodeInput model text =
    let
        uiState = model.uiState
    in
        { model
        | uiState =
            { uiState
            | newWorld = Just (text, parse "" text)
            }
        }


updateChangeCode : Model -> Model
updateChangeCode model =
    case model.world of
        Err _ ->
            model
        Ok w ->
            let
                uiState = model.uiState
                world =
                    uiState.newWorld
                        |> Maybe.withDefault ("", Ok w)
                        |> Tuple.second
                        |> Result.withDefault w
            in
                { model
                | world = Ok world
                , uiState =
                    { uiState
                    | newWorld = Nothing
                    , mode = InitialMode
                    }
                }


updateDetailsInput : Model -> String -> String -> Model
updateDetailsInput model name value =
    let
        uiState = model.uiState
    in
        { model
        | uiState =
            { uiState
            | newMetaLines = MetaLines.setDiff name value uiState.newMetaLines
            }
        }


updateChangeDetails : Model -> Model
updateChangeDetails model =
    case model.world of
        Err _ ->
            model
        Ok w ->
            let
                uiState = model.uiState
            in
                { model
                | world =
                    Ok
                        { w
                        | metaLines =
                            MetaLines.applyDiff
                                uiState.newMetaLines
                                w.metaLines
                        }
                , uiState =
                    { uiState
                    | mode = InitialMode
                    , newMetaLines = MetaLines.emptyDiff
                    }
                }


updateUndo : Model -> Model
updateUndo model =
    case model.past of
        [] -> model
        recent :: others ->
            { model
            | world = Ok recent
            , past = others
            , future =
                case model.world of
                    Ok w -> w :: model.future
                    _ -> model.future
            }


updateRedo : Model -> Model
updateRedo model =
    case model.future of
        [] -> model
        recent :: others ->
            { model
            | world = Ok recent
            , future = others
            , past =
                case model.world of
                    Ok w -> w :: model.past
                    _ -> model.past
            }


updateChangeBlock : Model -> Block -> Model
updateChangeBlock model block =
    let
        uiState = model.uiState
    in
        { model
        | uiState =
            { uiState
            | mode = PlaceBlockMode
            , block = Just block
            }
        }


updateChangeRabbit : Model -> Maybe Rabbit -> Model
updateChangeRabbit model rabbit =
    let
        uiState = model.uiState
    in
        { model
        | uiState =
            { uiState
            | mode = PlaceRabbitMode
            , rabbit = rabbit
            }
        }


updateChangeThing : Model -> Maybe Thing -> Model
updateChangeThing model thing =
    let
        uiState = model.uiState
    in
        { model
        | uiState =
            { uiState
            | mode = PlaceThingMode
            , thing = Just thing
            }
        }


-- Use the supplied function to update this Model's world,
-- unless this model has an invalid world, in which case
-- do nothing.
updateModelWorld : Model -> (World -> World) -> Model
updateModelWorld model fn =
    case model.world of
        Ok w -> {model | world = Ok (fn w)}
        _ -> model


updateLevelClickRabbit : Model -> Int -> Int -> Model
updateLevelClickRabbit model x y =
    updateModelWorld
        model
        (updateLevelClickRabbitWorld model.uiState.rabbit x y)


updateLevelClickRabbitWorld : Maybe Rabbit -> Int -> Int -> World -> World
updateLevelClickRabbitWorld newRabbit x y world =
    let
        rabbits =
            case newRabbit of
                Nothing ->
                    List.filter
                        (\rabbit -> rabbit.x /= x || rabbit.y /= y)
                        world.rabbits
                Just r ->
                    movedRabbit x y r :: world.rabbits
    in
        makeWorld
            world.comment
            world.blocks
            rabbits
            world.things
            world.metaLines


updateLevelClickThing : Model -> Int -> Int -> Model
updateLevelClickThing model x y =
    updateModelWorld model (updateLevelClickThingWorld model.uiState.thing x y)


updateLevelClickThingWorld :
    Maybe (Maybe Thing) ->
    Int ->
    Int ->
    World ->
    World
updateLevelClickThingWorld maybeNewThing x y world =
    let
        newThing : Maybe Thing
        newThing =
            case maybeNewThing of
                Nothing -> Just (Entrance 0 0)
                Just t -> t

        things : List (Thing)
        things =
            case newThing of
                Nothing ->
                    List.filter
                        (\thing -> Thing.pos thing /= (x, y))
                        world.things
                Just t ->
                    Thing.moved x y t :: world.things
    in
        makeWorld
            world.comment
            world.blocks
            world.rabbits
            things
            world.metaLines


updateLevelClickBlock : Model -> Int -> Int -> Model
updateLevelClickBlock model x y =
    updateModelWorld model (updateLevelClickBlockWorld model.uiState.block x y)


updateLevelClickBlockWorld : Maybe Block -> Int -> Int -> World -> World
updateLevelClickBlockWorld newBlock x y world =
    makeWorld
        world.comment
        (makeBlockGrid
            (List.indexedMap
                (updateLevelClickBlockRow newBlock x y) (blocks world))
        )
        world.rabbits
        world.things
        world.metaLines


updateLevelClickBlockRow :
    Maybe Block -> Int -> Int -> Int -> List Block -> List Block
updateLevelClickBlockRow newBlock x y rowy blocks =
    List.indexedMap (updateLevelClickBlockBlock newBlock x y rowy) blocks


updateLevelClickBlockBlock :
    Maybe Block -> Int -> Int -> Int -> Int -> Block -> Block
updateLevelClickBlockBlock newBlock x y rowy colx block =
    if x == colx && y == rowy then
        case newBlock of
            Nothing -> Block Earth Flat
            Just b  -> b
    else
        block


updateChangeMode : Model -> UiMode -> Model
updateChangeMode model mode =
    let
        uiState = model.uiState
        m =
            case mode of
                CodeMode _ ->
                    case model.world of
                        Ok w -> CodeMode (WorldTextRender.render w)
                        _ -> mode
                _ -> mode
        newMetaLines =
            case mode of
                ModifyDetailsMode ->
                    MetaLines.emptyDiff
                _ ->
                    uiState.newMetaLines
        newWorld =
            case mode of
                CodeMode _ -> Nothing
                _ -> uiState.newWorld
    in
        { model
        | uiState =
            { uiState
            | mode = m
            , newMetaLines = newMetaLines
            , newWorld = newWorld
            }
        }


updateAddColumn : Model -> Model
updateAddColumn model =
    case model.world of
        Err _ ->
            model
        Ok w ->
            { model
            | world =
                Ok
                    ( makeWorld
                        w.comment
                        ( makeBlockGrid
                            (List.map (\r -> r ++ [NoBlock]) (blocks w))
                        )
                        w.rabbits
                        w.things
                        w.metaLines
                    )
            }


updateRemoveColumn : Model -> Model
updateRemoveColumn model =
    updateModelWorld model updateWorldRemoveColumn


updateWorldRemoveColumn : World -> World
updateWorldRemoveColumn world =
    let
        bls = blocks world
        cols =
            case bls of
                [] -> 3
                h :: _ -> List.length h

        lastColRabbit : Rabbit -> Bool
        lastColRabbit rabbit =
            rabbit.x /= cols - 1

        lastColThing : Thing -> Bool
        lastColThing thing =
            let (x, _) = Thing.pos thing in
                x /= cols - 1
    in
        makeWorld
            world.comment
            ( makeBlockGrid
                ( List.map
                    (\r -> List.take ((List.length r) - 1) r)
                    bls
                )
            )
            ( List.filter lastColRabbit world.rabbits )
            ( List.filter lastColThing world.things )
            world.metaLines


updateAddRow : Model -> Model
updateAddRow model =
    updateModelWorld model updateWorldAddRow


updateWorldAddRow : World -> World
updateWorldAddRow world =
    let
        bls = blocks world
        cols =
            case bls of
                [] -> 3
                h :: _ -> List.length h
    in
        makeWorld
            world.comment
            ( makeBlockGrid
                (blocks world ++ [List.repeat cols NoBlock])
            )
            world.rabbits
            world.things
            world.metaLines


updateRemoveRow : Model -> Model
updateRemoveRow model =
    updateModelWorld model updateWorldRemoveRow


updateWorldRemoveRow : World -> World
updateWorldRemoveRow world =
    let
        bls = blocks world
        rows = List.length bls

        lastRowRabbit : Rabbit -> Bool
        lastRowRabbit rabbit =
            rabbit.y /= rows - 1

        lastRowThing : Thing -> Bool
        lastRowThing thing =
            let (_, y) = Thing.pos thing in
                y /= rows - 1
    in
        makeWorld
            world.comment
            ( makeBlockGrid (List.take (rows - 1) bls) )
            ( List.filter lastRowRabbit world.rabbits )
            ( List.filter lastRowThing world.things )
            world.metaLines
