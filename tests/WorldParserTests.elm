module WorldParserTests exposing
    ( all
    , toCharItemCases
    , mergeNewCharIntoItemsCases
    , starLineToItemsCases
    , integrateSquareCases
    , integrateLineCases
    , integrateLinesCases
    , metaLineCases
    , parseErrorCases
    )

import Test exposing (describe,test,Test)
import Expect


import Item2Text exposing (CharItem(..))
import MetaLines exposing (MetaLines)
import MetaValue exposing (MetaValue(..))
import ParseErr exposing (ParseErr(..))
import Rabbit exposing (Direction(..), Rabbit, makeRabbit)
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
import WorldParser exposing
    ( Items
    , StarLine
    , integrateSquare
    , integrateLine
    , integrateLines
    , resultCombine
    , mergeNewCharIntoItems
    , parse
    , parseErrToString
    , makeStarLine
    , starLineToItems
    , stringRemoveLastIfEmpty
    , toCharItem
    )


all : Test
all =
    describe "Tests of the world parser and manipulation"
        [ test "Combining good items make a good list" combiningGood
        , test "Combining a bad items makes a bad result" combiningBad
        , test "Combining no items makes a good empty list" combiningNone
        , test "Empty last line is removed" emptyLastLineIsRemoved
        , test "Parse empty world" parseEmptyWorld
        , test "Parse world with blocks" parseWorldWithBlocks
        , test "Parse world with rabbits" parseWorldWithRabbits
        , test "Parse world with things" parseWorldWithThings
        , test "Parse world with many hints" parseWorldWithManyHints
        , test "Parse world with gaps between hints" parseWorldWithHintsGaps
        , test "Parse bad world (long line)" parseBadWorldLongLine
        , test "Parse bad world (bad char)" parseBadWorldBadChar
        , test "Parse bad world (unknown list)" parseBadWorldUnknownList
        , test "Parse overlapping rabbits" parseOverlappingRabbits
        , test "Parse multiple stars" parseMultipleStars
        ]


-- ---


okAndEqual : Result ParseErr a -> a -> () -> Expect.Expectation
okAndEqual actual expected =
    \() ->
        case actual of
            Ok value -> Expect.equal value expected
            Err error -> Expect.fail ( parseErrToString error )


notOkAndEqual : Result ParseErr a -> String -> () -> Expect.Expectation
notOkAndEqual actual expected =
    \() ->
        case actual of
            Ok _ -> Expect.fail "Expected a parse error!"
            Err error -> Expect.equal ( parseErrToString error ) expected


parseLines : String -> List String -> Result ParseErr World
parseLines comment strings =
    parse comment ((String.join "\n" strings) ++ "\n")


fltErth : Block
fltErth =
    Block Earth Flat


uprErth : Block
uprErth =
    Block Earth UpRight


fltMetl : Block
fltMetl =
    Block Metal Flat


-- stringRemoveLastIfEmpty --


emptyLastLineIsRemoved : () -> Expect.Expectation
emptyLastLineIsRemoved =
    \() ->
        Expect.equal
            "a\nb"
            (stringRemoveLastIfEmpty "a\nb\n")


-- Combine --


combiningGood : () -> Expect.Expectation
combiningGood =
    \() ->
        Expect.equal
            (Ok ["a", "b", "c"])
            (resultCombine [Ok "a", Ok "b", Ok "c"])


combiningBad : () -> Expect.Expectation
combiningBad =
    \() ->
        Expect.equal
            (Err "xxx")
            (resultCombine [Err "xxx", Ok "b", Ok "c"])


combiningNone : () -> Expect.Expectation
combiningNone =
    \() ->
        Expect.equal
            (Ok [])
            (resultCombine [])

-- Merge --


emptyItems : Items
emptyItems = { block = NoBlock, rabbits = [], things = [] }


toCharItemCases : Test
toCharItemCases =
    let
        pos14 = { row = 4, col = 1 }
        pos36 = { row = 6, col = 3 }
        rabr14 = makeRabbit 1 4 Right
        rabl36 = makeRabbit 3 6 Left
        n = Nothing
        t desc gridPos pos char exp =
            test
                desc
                ( \() ->
                    Expect.equal
                        ( toCharItem gridPos pos.row pos.row pos.col char )
                        ( exp )
                )
    in
        describe "toCharItem"
            [ t "Sloping block" n pos14 '/' ( Ok (BlockChar pos14 uprErth ) )
            , t "Flat metal"    n pos36 'M' ( Ok (BlockChar pos36 fltMetl ) )
            , t "Right rabbit"  n pos14 'r' ( Ok (RabbitChar pos14 rabr14 ) )
            , t "Left rabbit"   n pos36 'j' ( Ok (RabbitChar pos36 rabl36 ) )
            , t "Star"          n pos14 '*' ( Ok ( StarChar pos14 pos14) )
            , t "Unknown" n pos14 '>' ( Err ( UnrecognisedChar pos14 '>' ) )

            , t "Rabbit at a different position"
               (Just pos36) pos14 'j' ( Ok (RabbitChar pos36 rabl36 ) )
            ]


mergeNewCharIntoItemsCases : Test
mergeNewCharIntoItemsCases =
    let
        mrg ch items = mergeNewCharIntoItems ch ( Ok items )
        blc = fltErth
        rab = makeRabbit 3 0 Right
        ra2 = makeRabbit 3 1 Left
        pos13 = { row = 3, col = 1 }
        pos24 = { row = 4, col = 2 }
        pos36 = { row = 6, col = 3 }
        staPos = { row = 4, col = 0 }
        rabPos = { row = rab.y, col = rab.x }
        ra2Pos = { row = ra2.y, col = ra2.x }
        blcCh = BlockChar pos24 blc
        rabCh = RabbitChar rabPos rab
        ra2Ch = RabbitChar ra2Pos ra2
        staCh = StarChar staPos staPos
        err = Err ( TwoBlocksInOneStarPoint pos36 '#' '#')
        t desc act exp = test desc (\() -> Expect.equal act exp)
    in
        describe "mergeNewCharIntoItems"
            [ t "Block merges into empty"
                ( mrg blcCh emptyItems )
                ( Ok { block = fltErth, rabbits = [], things = [] } )

            , t "Rabbit merges into empty"
                ( mrg rabCh emptyItems )
                ( Ok { block = NoBlock, rabbits = [rab], things = [] } )

            , t "Block won't merge over a block"
                ( mrg blcCh { emptyItems | block = fltMetl } )
                ( Err ( TwoBlocksInOneStarPoint pos24 'M' '#' ) )

            , t "Rabbit merges into block"
                ( mrg rabCh { emptyItems | block = fltMetl } )
                ( Ok { block = fltMetl, rabbits = [rab], things = [] } )

            , t "Rabbit merges into block + rabbit"
                ( mrg ra2Ch { block = fltMetl, rabbits = [rab], things = [] } )
                ( Ok { block = fltMetl, rabbits = [rab, ra2], things = [] } )

            , t "Errors propagate"
                ( mergeNewCharIntoItems ra2Ch err )
                ( err )

            , t "Star in starline is an error"
                ( mrg staCh emptyItems )
                ( Err ( StarInsideStarPoint staPos ) )
            ]


starLineToItemsCases : Test
starLineToItemsCases =
    let
        pos56 = { row = 6, col = 5 }
        pos59 = { row = 9, col = 5 }
        pos69 = { row = 9, col = 6 }
        rabl36 = makeRabbit 3 6 Left
        rabr45 = makeRabbit 4 5 Right
        rabr46 = makeRabbit 4 6 Right
        rabl56 = makeRabbit 5 6 Left
        rabr56 = makeRabbit 5 6 Right
        makeStarLineOk line =
            case makeStarLine line of
                Ok s -> s
                _ -> { row = -999, chars = ['p', 'a', 'r'] } -- parsing failed
        t desc inp row pos exp =
            test
                desc
                ( \() ->
                    Expect.equal
                        ( starLineToItems
                            ( makeStarLineOk { row = row, content = inp } )
                            pos
                        )
                        exp
                )
    in
        describe "starLineToItems"
            [ t "Single block"
                ":*=#" 9 pos56
                (Ok {block=fltErth, rabbits=[], things=[]})

            , t "Block and rabbit"
                ":*=/r" 9 pos56
                (Ok {block=uprErth, rabbits=[rabr56], things=[]})

            , t "Rabbits"
                ":*=jr" 9 pos56
                (Ok {block=NoBlock, rabbits=[rabl56, rabr56], things=[]})

            , t "Rabbits and block" ":*=jr#" 9 pos56
                (Ok {block=fltErth, rabbits=[rabl56, rabr56], things=[]})

            , t "Unknown char"
                ":*=jr#>" 9 pos56
                (Err (UnrecognisedChar pos69 '>'))

            , t "Star in star"
                ":*=jr*#" 9 pos56
                (Err (StarInsideStarPoint pos59))
            ]


integrateSquareCases : Test
integrateSquareCases =
    let
        pos11 = { row =  1, col = 1 }
        pos25 = { row =  5, col = 2 }
        pos88 = { row =  8, col = 8 }
        pos34 = { row =  4, col = 3 }
        pos39 = { row =  9, col = 3 }
        pos3a = { row = 10, col = 3 }
        blcErth11 = BlockChar pos11 fltErth
        rab25 = (makeRabbit 2 5 Right)
        rab34 = (makeRabbit 3 4 Right)
        rabCh25 = RabbitChar pos25 rab25
        star34 = StarChar pos34 pos34
        remainingStarLines =
            [ StarLine 9 (String.toList "rrj")
            ]
        starLines = StarLine 8 (String.toList "/r") :: remainingStarLines
        t desc act exp = test desc (\() -> Expect.equal act exp)
    in
        describe "integrateSquare"
            [ t "Block is left alone"
                (integrateSquare blcErth11 starLines)
                (Ok ({block=fltErth, rabbits=[], things=[]}, starLines))

            , t "Rabbit is left alone"
                (integrateSquare rabCh25 starLines)
                (Ok ({block=NoBlock, rabbits=[rab25], things=[]}, starLines))

            , t "Star integrates first starpoint"
                (integrateSquare star34 starLines)
                (Ok
                    ( {block = uprErth, rabbits=[rab34], things=[]}
                    , remainingStarLines)
                    )

            , t "Bad starline produces error"
                (integrateSquare star34 [ StarLine 3 ['*']])
                (Err (StarInsideStarPoint { row = 3, col = 3 }))
            ]


integrateLineCases : Test
integrateLineCases =
    let
        pos01 = { row = 1, col = 0 }
        pos11 = { row = 1, col = 1 }
        pos21 = { row = 1, col = 2 }
        pos22 = { row = 2, col = 2 }
        starLine1 = StarLine 10 (String.toList "rrj")
        starLine2 = StarLine 11 (String.toList "Mj")
        starLine3 = StarLine 12 (String.toList "#rj")
        starLines = [starLine1, starLine2, starLine3]
        rabr01 = makeRabbit 0 1 Right
        rabl01 = makeRabbit 0 1 Left
        rabl11 = makeRabbit 1 1 Left
        rabr21 = makeRabbit 2 1 Right
        rabl21 = makeRabbit 2 1 Left
        rabr22 = makeRabbit 2 2 Right
        chErth = BlockChar pos11 fltErth
        chRabb = RabbitChar pos11 rabl11
        t desc act expLines expStarLines =
            test desc (
                \() ->
                    Expect.equal
                        act
                        (Ok ({items=expLines, physicalRow=1}, expStarLines))
            )
        tbad desc act exp = test desc (\() -> Expect.equal act (Err exp))
    in
        describe "integrateLine"
            [ t "Non-stars are left alone"
                (integrateLine [chErth, chRabb, chErth] starLines)
                [ { block=fltErth, rabbits=[], things=[] }
                , { block=NoBlock, rabbits=[rabl11], things=[] }
                , { block=fltErth, rabbits=[], things=[] }
                ]
                starLines

            , t "Star gets integrated"
                (integrateLine [StarChar pos01 pos01, chRabb, chErth] starLines)
                [ { block=NoBlock
                  , rabbits=[rabr01, rabr01, rabl01]
                  , things=[]
                  }
                , { block=NoBlock, rabbits=[rabl11], things=[] }
                , { block=fltErth, rabbits=[], things=[] }
                ]
                [starLine2, starLine3]

            , t "Multiple stars get integrated"
                (integrateLine
                    [ StarChar pos01 pos01
                    , StarChar pos11 pos11
                    , StarChar pos21 pos21
                    ]
                    starLines
                )
                [ { block=NoBlock
                  , rabbits=[rabr01, rabr01, rabl01]
                  , things=[]
                  }
                , { block=fltMetl, rabbits=[rabl11], things=[] }
                , { block=fltErth
                  , rabbits=[rabr21, rabl21]
                  , things=[]
                  }
                ]
                []

            , tbad "Bad star produces an error"
                (integrateLine
                    [StarChar pos01 pos01, chErth, chErth]
                    [StarLine 10 (String.toList "MM")] -- two blocks
                )
                ( TwoBlocksInOneStarPoint { row = 10, col = 4 } 'M' 'M' )
            ]


integrateLinesCases : Test
integrateLinesCases =
    let
        pos10 = { row = 0, col = 1 }
        chErth = BlockChar pos10 fltErth
        rabl10 = makeRabbit 1 0 Left
        chRabb10 = RabbitChar pos10 rabl10
        pos01 = { row = 1, col = 0 }
        rabr01 = makeRabbit 0 1 Right
        rabl01 = makeRabbit 0 1 Left
        pos11 = { row = 1, col = 1 }
        rabl11 = makeRabbit 1 1 Left
        pos12 = { row = 2, col = 1 }
        rabr12 = makeRabbit 1 2 Right
        chRabb12 = RabbitChar pos12 rabr12
        pos21 = { row = 1, col = 2 }
        rabr21 = makeRabbit 2 1 Right
        rabl21 = makeRabbit 2 1 Left
        pos02 = { row = 2, col = 0 }
        rabl02 = makeRabbit 0 2 Left
        rabr02 = makeRabbit 0 2 Right
        starLine1 = StarLine 10 (String.toList "rrj")
        starLine2 = StarLine 11 (String.toList "Mj")
        starLine3 = StarLine 12 (String.toList "#rj")
        starLine4 = StarLine 13 (String.toList "#rj")
        starLine5 = StarLine 14 (String.toList "#rj")
        starLines = [starLine1, starLine2, starLine3, starLine4, starLine5]
        t desc act exp = test desc (\() -> Expect.equal act exp)
        itRow physicalRow items = {physicalRow=physicalRow, items=items}
    in
        describe "integrateLines"
            [ t "Multiple lines"
                (integrateLines
                    [ [chErth, chRabb10, chErth]
                    , [ StarChar pos01 pos01
                      , StarChar pos11 pos11
                      , StarChar pos21 pos21
                      ]
                    , [ StarChar pos02 pos02
                      , chRabb12
                      , chErth
                      ]
                    ]
                    starLines
                )
                ( Ok
                    (
                        [ itRow
                            0
                            [ { block=fltErth, rabbits=[], things=[] }
                            , { block=NoBlock, rabbits=[rabl10], things=[] }
                            , { block=fltErth, rabbits=[], things=[] }
                            ]
                        , itRow
                            1
                            [ { block=NoBlock
                              , rabbits=[rabr01, rabr01, rabl01]
                              , things=[]
                              }
                            , { block=fltMetl, rabbits=[rabl11], things=[] }
                            , { block=fltErth
                              , rabbits=[rabr21, rabl21]
                              , things=[]
                              }
                            ]
                        , itRow
                            2
                            [ { block=fltErth
                              , rabbits=[rabr02, rabl02]
                              , things=[]
                              }
                            , { block=NoBlock, rabbits=[rabr12], things=[] }
                            , { block=fltErth, rabbits=[], things=[] }
                            ]
                        ]
                    , [starLine5]
                    )
                )

            , t "Bad star produces an error"
                (integrateLines
                    [ [chErth, chRabb10, chErth]
                    , [StarChar pos01 pos01, chErth, chErth]
                    ]
                    [StarLine 10 (String.toList "Mr*")] -- star in star
                )
                ( Err
                    ( StarInsideStarPoint { row = 10, col = 5 } )
                )
            ]


-- Parse --


parseEmptyWorld : () -> Expect.Expectation
parseEmptyWorld =
    okAndEqual
        (parseLines
            "tst"
            [ "   "
            , "   "
            , "   "
            ]
        )
        (makeWorld
            "tst"
            (makeBlockGrid
                [ [NoBlock, NoBlock, NoBlock]
                , [NoBlock, NoBlock, NoBlock]
                , [NoBlock, NoBlock, NoBlock]
                ]
            )
            []
            []
            MetaLines.defaults
            (WaterLines [])
        )


parseWorldWithBlocks : () -> Expect.Expectation
parseWorldWithBlocks =
    okAndEqual
        (parseLines
            "tst"
            [ "    "
            , "   #"
            , "    "
            , "####"
            ]
        )
        (makeWorld
            "tst"
            (makeBlockGrid
                [ [NoBlock, NoBlock, NoBlock, NoBlock]
                , [NoBlock, NoBlock, NoBlock, fltErth]
                , [NoBlock, NoBlock, NoBlock, NoBlock]
                , [fltErth, fltErth, fltErth, fltErth]
                ]
            )
            []
            []
            MetaLines.defaults
            (WaterLines [])
        )


parseWorldWithRabbits : () -> Expect.Expectation
parseWorldWithRabbits =
    okAndEqual
        (parseLines
            "tst"
            [ ":name=xyz"
            , ":description=copious"
            , "   j"
            , "   #"
            , "r   "
            , "####"
            ]
        )
        (makeWorld
            "tst"
            (makeBlockGrid
                [ [NoBlock, NoBlock, NoBlock, NoBlock]
                , [NoBlock, NoBlock, NoBlock, fltErth]
                , [NoBlock, NoBlock, NoBlock, NoBlock]
                , [fltErth, fltErth, fltErth, fltErth]
                ]
            )
            [ makeRabbit 3 0 Left
            , makeRabbit 0 2 Right
            ]
            []
            ( MetaLines.fromList
                [ ( "name", MvString "xyz" )
                , ( "description", MvString "copious" )
                ]
            )
            (WaterLines [])
        )


parseWorldWithThings : () -> Expect.Expectation
parseWorldWithThings =
    okAndEqual
        (parseLines
            "tst"
            [ ":name=foo"
            , ":bash=1"
            , ":block=2"
            , "Q  j"
            , "   #"
            , "rO  "
            , "####"
            ]
        )
        (makeWorld
            "tst"
            (makeBlockGrid
                [ [NoBlock, NoBlock, NoBlock, NoBlock]
                , [NoBlock, NoBlock, NoBlock, fltErth]
                , [NoBlock, NoBlock, NoBlock, NoBlock]
                , [fltErth, fltErth, fltErth, fltErth]
                ]
            )
            [ makeRabbit 3 0 Left
            , makeRabbit 0 2 Right
            ]
            [ Entrance 0 0
            , Exit 1 2
            ]
            ( MetaLines.fromList
                [ ( "name", MvString "foo" )
                , ( "bash", MvInt 1 )
                , ( "block", MvInt 2 )
                ]
            )
            (WaterLines [])
        )


parseWorldWithManyHints : () -> Expect.Expectation
parseWorldWithManyHints =
    okAndEqual
        (parseLines
            "tst"
            [ ":hint.1=foo"
            , ":hint.2=bar"
            , ":hint.3=baz"
            , ":hint.4=qux"
            , "#"
            ]
        )
        (makeWorld
            "tst"
            (makeBlockGrid
                [ [fltErth]
                ]
            )
            []
            []
            ( MetaLines.fromList
                [ ( "hint", MvList ["foo", "bar", "baz", "qux"] )
                ]
            )
            (WaterLines [])
        )


parseWorldWithHintsGaps : () -> Expect.Expectation
parseWorldWithHintsGaps =
    okAndEqual
        (parseLines
            "tst"
            [ ":hint.1=foo"
            , ":hint.4=qux"
            , "#"
            ]
        )
        (makeWorld
            "tst"
            (makeBlockGrid
                [ [fltErth]
                ]
            )
            []
            []
            ( MetaLines.fromList
                [ ( "hint", MvList ["foo", "", "", "qux"] )
                ]
            )
            (WaterLines [])
        )


parseBadWorldLongLine : () -> Expect.Expectation
parseBadWorldLongLine =
    notOkAndEqual
        ( parseLines
            "tst"
            [ ":name=foo"
            , ":bash=1"
            , ":block=2"
            , ":brolly=1"
            , "Q  j#"
            , "   #"
            , "rO  "
            , "####"
            ]
        )
        (  "Line 6, column 4: The lines of this level are different "
        ++ "lengths - they must be all exactly the same length.  "
        ++ "The first line was 5 characters long, but this one is 4."
        )


parseBadWorldBadChar : () -> Expect.Expectation
parseBadWorldBadChar =
    notOkAndEqual
        ( parseLines
            "tst"
            [ ":name=foo"
            , ":bash=1"
            , ":block=2"
            , ":brolly=1"
            , "Q  ~"
            , "   #"
            , "rO  "
            , "####"
            ]
        )
        "Line 5, column 4: Unrecognised character: '~'."


parseBadWorldUnknownList : () -> Expect.Expectation
parseBadWorldUnknownList =
    notOkAndEqual
        ( parseLines
            "tst"
            [ ":name.1=foo"
            , " "
            ]
        )
        ( "Line 1, column 1: Unknown meta property name 'name.1'." ++
          "  (The value provided was 'foo')."
        )


parseOverlappingRabbits : () -> Expect.Expectation
parseOverlappingRabbits =
    okAndEqual
        (parseLines
            "tst"
            [ ":name=bar"
            , "   *"
            , "   #"
            , "    "
            , "####"
            , ":*=rj"
            ]
        )
        (makeWorld
            "tst"
            (makeBlockGrid
                [ [NoBlock, NoBlock, NoBlock, NoBlock]
                , [NoBlock, NoBlock, NoBlock, fltErth]
                , [NoBlock, NoBlock, NoBlock, NoBlock]
                , [fltErth, fltErth, fltErth, fltErth]
                ]
            )
            [ makeRabbit 3 0 Right
            , makeRabbit 3 0 Left
            ]
            []
            ( MetaLines.fromList
                [ ( "name", MvString "bar" )
                ]
            )
            (WaterLines [])
        )


parseMultipleStars : () -> Expect.Expectation
parseMultipleStars =
    okAndEqual
        (parseLines
            "tst"
            [ "   *"
            , " **#"
            , "    "
            , "####"
            , ":*=rjQ"
            , ":*=#rj"
            , ":*=/jrO"
            ]
        )
        (makeWorld
            "tst"
            (makeBlockGrid
                [ [NoBlock, NoBlock, NoBlock, NoBlock]
                , [NoBlock, fltErth, uprErth, fltErth]
                , [NoBlock, NoBlock, NoBlock, NoBlock]
                , [fltErth, fltErth, fltErth, fltErth]
                ]
            )
            [ makeRabbit 3 0 Right
            , makeRabbit 3 0 Left
            , makeRabbit 1 1 Right
            , makeRabbit 1 1 Left
            , makeRabbit 2 1 Left
            , makeRabbit 2 1 Right
            ]
            [ Entrance 3 0
            , Exit 2 1
            ]
            MetaLines.defaults
            (WaterLines [])
        )


parseErrorCases : Test
parseErrorCases =
    let
        t desc act exp =
            test desc ( \() -> Expect.equal (parseLines desc act) (Err exp) )
    in
        describe "parse errors"
            [ t "Invalid character in grid"
                [ "   >"
                , "    "
                ]
                ( UnrecognisedChar {row=0, col=3} '>' )

            , t "Invalid character in star line"
                [ "   *"
                , "    "
                , ":*=rrrr<"
                ]
                ( UnrecognisedChar {row=2, col=7} '<' )

            , t "Not enough star lines"
                [ "   *"
                , " *  "
                , ":*=rrrr"
                ]
                ( NotEnoughStarLines {row=1, col=1} )

            , t "Too many star lines"
                [ "    "
                , " *  "
                , ":*=rrrr"
                , ":*=rrrr"
                ]
                ( TooManyStarLines {row=3, col=0} )

            , t "Two blocks in one star point"
                [ "    "
                , " *  "
                , ":*=r#r/"
                ]
                ( TwoBlocksInOneStarPoint {row=2, col=6} '#' '/' )

            , t "Star inside a star line"
                [ "    "
                , "    "
                , " *  "
                , ":*=*"
                ]
                ( StarInsideStarPoint {row=3, col=3} )

            , t "Unrecognised colon line"
                [ "  "
                , "  "
                , ":foo=bar"
                ]
                ( UnknownMetaName {row=2, col=0} "foo" "bar" )

            , t "Line too long"
                [ "  "
                , "      "
                , "  "
                ]
                ( LineWrongLength {row=1, col=2} 2 6 )

            , t "Line too short"
                [ "  "
                , " "
                , "  "
                ]
                ( LineWrongLength {row=1, col=0} 2 1 )
            ]


type alias Defn =
    { blocks : List (List Block)
    , rabbits : List Rabbit
    , things : List Thing
    , metaLines : MetaLines
    , waterLines : WaterLines
    }


metaLineCases : Test
metaLineCases =
    let
        t
            :  String
            -> List String
            -> Defn
            -> Test
        t desc act defn =
            test
                desc
                ( \() ->
                    Expect.equal
                        (parseLines desc act)
                        ( Ok
                            ( makeWorld
                                desc
                                (makeBlockGrid defn.blocks)
                                defn.rabbits
                                defn.things
                                defn.metaLines
                                defn.waterLines
                            )
                        )
                )
    in
        describe "Meta-lines"
            [ t "Meta-line on its own"
                [ ":num_rabbits=3"
                ]
                { blocks = []
                , rabbits = []
                , things = []
                , metaLines = MetaLines.fromList [("num_rabbits", MvInt 3)]
                , waterLines = WaterLines []
                }

            , t "Meta-line with grid"
                [ "##"
                , "# "
                , ":num_rabbits=3"
                , ":num_to_save=2"
                ]
                { blocks =
                    [ [fltErth, fltErth]
                    , [fltErth, NoBlock]
                    ]
                , rabbits = []
                , things = []
                , metaLines =
                    MetaLines.fromList
                        [ ("num_rabbits", MvInt 3)
                        , ("num_to_save", MvInt 2)
                        ]
                , waterLines = WaterLines []
                }

            , t "Water"
                [ " Nn"
                , "*  "
                , ":*=/n"
                ]
                { blocks =
                    [ [NoBlock, NoBlock, NoBlock]
                    , [uprErth, NoBlock, NoBlock]
                    ]
                , rabbits = []
                , things =
                    [ WaterRegion 1 0 Full
                    , WaterRegion 2 0 Half
                    , WaterRegion 0 1 Half
                    ]
                , metaLines = MetaLines.defaults
                , waterLines = WaterLines []
                }

            , t "Pipe"
                [ "  P"
                , "*  "
                , ":*=/P"
                ]
                { blocks =
                    [ [NoBlock, NoBlock, NoBlock]
                    , [uprErth, NoBlock, NoBlock]
                    ]
                , rabbits = []
                , things =
                    [ Pipe 2 0
                    , Pipe 0 1
                    ]
                , metaLines = MetaLines.defaults
                , waterLines = WaterLines []
                }

            , t "Water amounts"
                [ " "
                , ":n=3,4,198"
                , ":n=0,4,199"
                ]
                { blocks =
                    [ [NoBlock]
                    ]
                , rabbits = []
                , things = []
                , metaLines = MetaLines.defaults
                , waterLines = WaterLines [":n=3,4,198", ":n=0,4,199"]
                }
            ]
