module WorldRoundTripTests exposing (all)

import Test exposing (describe,test,Test)
import Expect


import ParseErr exposing (ParseErr)
import World exposing (World)
import WorldParser exposing (parse, parseErrToString)
import WorldTextRender exposing (render)


all : Test
all =
    describe "Tests for round-trips of worlds as text"
        [ test "Just blocks"
            ( roundTrips
                [ "   "
                , "#  "
                , " ##"
                ]
            )

        , test "Slopes"
            ( roundTrips
                [ "  /"
                , "#\\ "
                , " ##"
                ]
            )

        , test "Bridges"
            ( roundTrips
                [ "/ ("
                , "#) "
                , " ##"
                ]
            )

        , test "Metal"
            ( roundTrips
                [ "/ ("
                , "#) "
                , " MM"
                ]
            )

        , test "Rabbits"
            ( roundTrips
                [ "/ ("
                , "#r "
                , "jMM"
                ]
            )

        , test "Starpoints"
            ( roundTrips
                [ "/ *"
                , "#* "
                , "jMM"
                , ":*=)rjrj"
                , ":*=#jj"
                ]
            )

        , test "Water"
            ( roundTrips
                [ "NnP"
                , "***"
                , ":*=)NP"
                , ":*=(n"
                , ":*=bn"
                ]
            )

        , test "Water amounts"
            ( roundTrips
                [ "NNN"
                , "NNN"
                , ":n=0,1,15000"
                , ":n=2,0,16000"
                ]
            )

        , test "Meta lines"
            ( roundTrips
                [ " "
                , ":name=My Level"
                , ":solution.1=bash;(1,2)"
                , ":solution.2=bash;(1,3)"
                , ":solution.3=bash;(1,4)"
                , ":solution.4=bash&(1,5);until:WON"
                ]
            )

        ]


-- ---


parseLines : String -> List String -> Result ParseErr World
parseLines comment strings =
    parse comment ((String.join "\n" strings) ++ "\n")


renderToLines : World -> List String
renderToLines world =
    String.split "\n" (render world)


roundTrips : List String -> () -> Expect.Expectation
roundTrips text =
    \() ->
        case parseLines "test" text of
            Ok world -> Expect.equal text (renderToLines world)
            Err error ->
                Expect.fail
                    ( "Parsing failed: "
                    ++ ( parseErrToString error )
                    )
