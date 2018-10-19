module World exposing
    ( Block(..)
    , BlockMaterial(..)
    , BlockShape(..)
    , Grid
    , World
    , addThing
    , addRabbit
    , blockAt
    , blocks
    , changeBlock
    , makeBlockGrid
    , makeWorld
    , rabbitsAt
    , thingsAt
    , width
    )


import MetaLines exposing (MetaLines)
import Rabbit exposing (Rabbit)
import Thing exposing (Thing)


type BlockMaterial =
    Earth | Metal


type BlockShape =
    Flat | UpRight | UpLeft | BridgeUpRight | BridgeUpLeft


type Block =
    NoBlock | Block BlockMaterial BlockShape


type Grid a =
    Grid Int Int (List (List a))


type alias World =
    { comment : String
    , blocks : Grid Block
    , rabbits : List Rabbit
    , things : List Thing
    , metaLines : MetaLines
    }


initBlocks : List (List Block)
initBlocks =
    [
        [ Block Earth Flat
        , Block Earth Flat
        ]
    ]


makeWorld :
    String ->
    Grid Block ->
    List Rabbit ->
    List Thing ->
    MetaLines ->
    World
makeWorld comment blocksGrid rabbits things metaLines =
    { comment = comment
    , blocks = blocksGrid
    , rabbits = rabbits
    , things = things
    , metaLines = metaLines
    }


makeBlockGrid : List (List Block) -> Grid Block
makeBlockGrid blocksList =
    case blocksList of
        [] -> Grid 0 0 blocksList
        x :: _ -> Grid (List.length x) (List.length blocksList) blocksList


-- How many blocks across this world
width : World -> Int
width world =
    case world.blocks of
        Grid x _ _ -> x


-- Extract the blocks from a world
blocks : World -> List (List Block)
blocks world =
    case world.blocks of
        Grid w h ls -> ls


-- Give me all rabbits at a co-ordinate
rabbitsAt : World -> Int -> Int -> List Rabbit
rabbitsAt world x y =
    List.filter (\r -> r.x == x && r.y == y) world.rabbits


thingsAt : World -> Int -> Int -> List Thing
thingsAt world x y =
    List.filter (\t -> Thing.pos t == (x, y)) world.things


blockAt : World -> Int -> Int -> Maybe Block
blockAt w x y =
    let
        bs = blocks w
    in
        case List.head (List.drop y bs) of
            Just row -> List.head (List.drop x row)
            _ -> Nothing


-- Return a world identical to the one supplied except
-- with the supplied block at the supplied co-ordinates
-- instead of what is there in the supplied world.
changeBlock : World -> Int -> Int -> Block -> World
changeBlock world x y block =
    let
        replBlockSingle : Int -> Block -> Block
        replBlockSingle currentX blockToReplace =
            if currentX == x then
                block
            else
                blockToReplace

        replBlockInRow : Int -> List Block -> List Block
        replBlockInRow currentY row =
            if currentY == y then
                List.indexedMap replBlockSingle row
            else
                row
    in
        makeWorld
            world.comment
            ( makeBlockGrid
                (List.indexedMap replBlockInRow (blocks world))
            )
            world.rabbits
            world.things
            world.metaLines


addThing : World -> Thing -> World
addThing world thing =
    makeWorld
        world.comment
        world.blocks
        world.rabbits
        (thing :: world.things)
        world.metaLines


addRabbit : World -> Rabbit -> World
addRabbit world rabbit =
    makeWorld
        world.comment
        world.blocks
        (rabbit :: world.rabbits)
        world.things
        world.metaLines
