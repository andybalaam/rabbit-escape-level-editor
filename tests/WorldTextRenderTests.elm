module WorldTextRenderTests exposing (all)

import Test exposing (describe,test,Test)
import Expect


import MetaLines exposing (MetaLines)
import MetaValue exposing (MetaValue(..))
import Rabbit exposing (Direction(..), Rabbit, makeRabbit, makeRabbot)
import Thing exposing (Thing(..), WaterContents(..))
import WaterLines exposing (WaterLines(..))
import World exposing
    ( World
    , Block(..)
    , BlockMaterial(..)
    , BlockShape(..)
    , makeBlockGrid
    , makeWorld
    )
import WorldTextRender exposing (render)


all : Test
all =
    describe "Tests of the world renderer"
        [ t "Render empty world"
            [ [NoBlock, NoBlock, NoBlock]
            , [NoBlock, NoBlock, NoBlock]
            , [NoBlock, NoBlock, NoBlock]
            ] [] [] MetaLines.defaults (WaterLines [])
            [ "   "
            , "   "
            , "   "
            ]

        , t "Render world with blocks"
            [ [NoBlock, NoBlock, NoBlock, NoBlock]
            , [NoBlock, NoBlock, NoBlock, fltErth]
            , [NoBlock, NoBlock, NoBlock, NoBlock]
            , [fltErth, fltErth, fltErth, fltErth]
            ] [] [] MetaLines.defaults (WaterLines [])
            [ "    "
            , "   #"
            , "    "
            , "####"
            ]

        , t "Render world with rabbits"
            [ [NoBlock, NoBlock, NoBlock, NoBlock]
            , [NoBlock, NoBlock, NoBlock, fltErth]
            , [NoBlock, NoBlock, NoBlock, NoBlock]
            , [fltErth, fltErth, fltErth, fltErth]
            ]
            [ makeRabbot 0 1 Right
            , makeRabbot 0 0 Left
            , makeRabbit 2 1 Right
            , makeRabbit 1 2 Left
            ] [] MetaLines.defaults (WaterLines [])
            [ "y   "
            , "t r#"
            , " j  "
            , "####"
            ]

        , t "Render world with things"
            [ [NoBlock, NoBlock, NoBlock, NoBlock]
            , [NoBlock, NoBlock, NoBlock, fltErth]
            , [NoBlock, NoBlock, NoBlock, NoBlock]
            , [fltErth, fltErth, fltErth, fltErth]
            ]
            [ makeRabbot 0 1 Right
            , makeRabbot 0 0 Left
            , makeRabbit 2 1 Right
            , makeRabbit 1 2 Left
            ]
            [ Entrance 1 0
            , Exit 3 0
            ]
            MetaLines.defaults
            (WaterLines [])
            [ "yQ O"
            , "t r#"
            , " j  "
            , "####"
            ]

        , t "Render 2 rabbits in same place"
            [ [NoBlock, NoBlock, NoBlock, NoBlock]
            , [NoBlock, NoBlock, NoBlock, NoBlock]
            , [NoBlock, NoBlock, NoBlock, NoBlock]
            , [NoBlock, NoBlock, NoBlock, NoBlock]
            ]
            [ makeRabbit 2 1 Right
            , makeRabbit 2 1 Left
            ] [] MetaLines.defaults (WaterLines [])
            [ "    "
            , "  * "
            , "    "
            , "    "
            , ":*=rj"
            ]

        , t "Render multiple things in same place"
            [ [NoBlock, NoBlock, NoBlock, NoBlock]
            , [NoBlock, NoBlock, fltErth, fltMetl]
            , [uprErth, NoBlock, NoBlock, NoBlock]
            , [NoBlock, NoBlock, NoBlock, NoBlock]
            ]
            [ makeRabbit 2 1 Right
            , makeRabbit 2 1 Left
            , makeRabbit 3 1 Right
            , makeRabbit 0 2 Left
            ]
            [ Entrance 2 1
            , Exit 3 1
            ]
            MetaLines.defaults
            (WaterLines [])
            [ "    "
            , "  **"
            , "*   "
            , "    "
            , ":*=#rjQ"
            , ":*=MrO"
            , ":*=/j"
            ]

        , t "Render meta-lines"
            [ [NoBlock]
            , [uprErth]
            ]
            []
            []
            ( MetaLines.fromList
                [ ("num_rabbits", MvInt 4)
                , ("num_to_save", MvInt 2)
                , ("solution.code", MvList ["a", "b", "c", "d"])
                ]
            )
            (WaterLines [])
            [ " "
            , "/"
            , ":solution.1.code=a"
            , ":solution.2.code=b"
            , ":solution.3.code=c"
            , ":solution.4.code=d"
            , ":num_rabbits=4"
            , ":num_to_save=2"
            ]


        , t "Render water"
            [ [NoBlock, NoBlock]
            , [NoBlock, uprErth]
            ]
            []
            [ WaterRegion 0 0 Full
            , WaterRegion 1 0 Half
            , WaterRegion 1 1 Half
            ]
            MetaLines.defaults
            (WaterLines [])
            [ "Nn"
            , " *"
            , ":*=/n"
            ]


        , t "Render pipe"
            [ [NoBlock, NoBlock]
            , [NoBlock, uprErth]
            ]
            []
            [ Pipe 1 0
            , Pipe 1 1
            ]
            MetaLines.defaults
            (WaterLines [])
            [ " P"
            , " *"
            , ":*=/P"
            ]


        , t "Render water amounts"
            [ [NoBlock]
            ]
            []
            []
            MetaLines.defaults
            (WaterLines [":n=2,3,150", ":n=0,0,133"])
            [ " "
            , ":n=2,3,150"
            , ":n=0,0,133"
            ]
        ]


-- ---


renderToLines : World -> List String
renderToLines world =
    String.split "\n" (render world)


fltErth : Block
fltErth =
    Block Earth Flat


fltMetl : Block
fltMetl =
    Block Metal Flat


uprErth : Block
uprErth =
    Block Earth UpRight


rend
    : List (List Block)
    -> List Rabbit
    -> List Thing
    -> MetaLines
    -> WaterLines
    -> List String
rend blocks rabbits things metaLines waterLines =
    renderToLines
        ( makeWorld
            "tst"
            (makeBlockGrid blocks)
            rabbits
            things
            metaLines
            waterLines
        )


t
    : String
    -> List (List Block)
    -> List Rabbit
    -> List Thing
    -> MetaLines
    -> WaterLines
    -> List String
    -> Test
t desc blocks rabbits things metaLines waterLines expected =
    test
        desc
        ( \() ->
            Expect.equal
                expected (rend blocks rabbits things metaLines waterLines)
        )
