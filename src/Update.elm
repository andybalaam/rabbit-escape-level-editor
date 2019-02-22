module Update exposing (update)


import Json.Encode as E


import MetaDiff
import MetaLines exposing (MetaLines)
import Model exposing (Item(..), Model, UiMode(..), ViewMode(..))
import Msg exposing (Msg(..))
import ParseErr exposing (ParseErr)
import Ports exposing (saveAndQuit)
import Rabbit exposing (Rabbit, movedRabbit)
import Thing exposing (Thing(..))
import World exposing
    ( Block(..)
    , BlockMaterial(..)
    , BlockShape(..)
    , World
    , addThing
    , addRabbit
    , blocks
    , changeBlock
    , makeWorld
    , makeBlockGrid
    )
import WorldParser exposing (parse)
import WorldTextRender


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SaveAndQuit ->
            let
                w : String
                w =
                    case model.world of
                        Ok world -> WorldTextRender.render world
                        Err (_, txt) -> txt
            in
                (model, saveAndQuit (E.string w))
        _->
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
                            updateLevelClick model x y
                        PlaceItemMode ->
                            updateLevelClick model x y
                        DeleteMode ->
                            updateLevelDelete model x y
                        _ ->
                            model
                ChangeItem item ->
                    updateChangeItem model item
                ChangeMode mode ->
                    updateChangeMode model mode
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
                ToggleFullScreen ->
                    updateToggleFullScreen model
                Undo ->
                    model  -- Should never happen - covered in update
                Redo ->
                    model  -- Should never happen - covered in update
                SaveAndQuit ->
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

updateChangeItem : Model -> Item -> Model
updateChangeItem model item =
    let
        uiState = model.uiState
    in
        { model
        | uiState =
            { uiState
            | item = Just item
            , mode = PlaceItemMode
            }
        }


updateChangeCode : Model -> Model
updateChangeCode model =
    let
        changedWorld : Result (ParseErr, String) World
        changedWorld =
            case uiState.newWorld of
                Just (_, Ok w) -> Ok w
                Just (_, Err e) -> Err (e, "")
                Nothing -> model.world

        uiState = model.uiState
    in
        { model
        | world = changedWorld
        , uiState =
            { uiState
            | newWorld = Nothing
            , mode = InitialMode
            }
        }


updateToggleFullScreen : Model -> Model
updateToggleFullScreen model =
    let
        uiState = model.uiState
        viewMode = uiState.viewMode
    in
        { model
        | uiState =
            { uiState
            | viewMode =
                case viewMode of
                    FullScreen -> Normal
                    Normal -> FullScreen
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
            | newMetaLines = MetaDiff.setDiff name value uiState.newMetaLines
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
                            MetaDiff.applyDiff
                                uiState.newMetaLines
                                w.metaLines
                        }
                , uiState =
                    { uiState
                    | mode = InitialMode
                    , newMetaLines = MetaDiff.emptyDiff
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


-- Use the supplied function to update this Model's world,
-- unless this model has an invalid world, in which case
-- do nothing.
updateModelWorld : Model -> (World -> World) -> Model
updateModelWorld model fn =
    case model.world of
        Ok w -> {model | world = Ok (fn w)}
        _ -> model


updateLevelDelete : Model -> Int -> Int -> Model
updateLevelDelete model x y =
    let
        newWorld : World -> World
        newWorld w = 
            if hasRabbit x y w then
                removeRabbitsAt w x y
            else if hasThing x y w then
                removeThingsAt w x y
            else
                changeBlock w x y NoBlock
    in
        updateModelWorld model newWorld


removeRabbitsAt : World -> Int -> Int -> World
removeRabbitsAt world x y =
    makeWorld
        world.comment
        world.blocks
        (List.filter (\r -> r.x /= x || r.y /= y) world.rabbits)
        world.things
        world.metaLines
        world.waterLines


removeThingsAt : World -> Int -> Int -> World
removeThingsAt world x y =
    makeWorld
        world.comment
        world.blocks
        world.rabbits
        (List.filter (\t -> Thing.pos t /= (x, y)) world.things)
        world.metaLines
        world.waterLines


hasRabbit : Int -> Int -> World -> Bool
hasRabbit x y world =
    List.any (\r -> r.x == x && r.y == y) world.rabbits


hasThing : Int -> Int -> World -> Bool
hasThing x y world =
    List.any (\t -> Thing.pos t == (x, y)) world.things


changeOrToggleBlock : World -> Int -> Int -> Block -> World
changeOrToggleBlock w x y block =
    let
        actualBlock =
            if World.blockAt w x y == Just block then
                NoBlock
            else
                block
    in
        changeBlock w x y actualBlock


updateLevelClick : Model -> Int -> Int -> Model
updateLevelClick model x y =
    let
        newWorld : World -> World
        newWorld w =
            case model.uiState.item of
                Nothing ->
                    changeOrToggleBlock w x y (Block Earth Flat)
                Just (BlockItem b) ->
                    changeOrToggleBlock w x y b
                Just (ThingItem t) ->
                    addThing w (Thing.moved x y t)
                Just (RabbitItem r) ->
                    addRabbit w (movedRabbit x y r)
    in
        updateModelWorld model newWorld


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
                    MetaDiff.emptyDiff
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
                        w.waterLines
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
            world.waterLines


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
            world.waterLines


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
            world.waterLines
