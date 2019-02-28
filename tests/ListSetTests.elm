module ListSetTests exposing (all)

import Test exposing (test, Test)
import Expect


import ListSet exposing (..)


all : Test
all = Test.concat
    [ eq "Padding a list that is just long enough does nothing"
        ["1", "2", "3"]
        (listPadToLength 3 ["1", "2", "3"])

    , eq "Padding a list that is long enough does nothing"
        ["1", "2", "3"]
        (listPadToLength 1 ["1", "2", "3"])

    , eq "Padding a list by zero does nothing"
        []
        (listPadToLength 0 [])

    , eq "Padding an empty list adds empty strings"
        ["", ""]
        (listPadToLength 2 [])

    , eq "Padding a non-empty list adds empty strings"
        ["1", "2", "", ""]
        (listPadToLength 4 ["1", "2"])

    , eq "Setting at beginning of empty list makes singleton"
        ["0"]
        (listSet 0 "0" [])

    , eq "Setting later in empty list makes padded single"
        ["", "", "2"]
        (listSet 2 "2" [])

    , eq "Setting later in non-empty list pads then adds"
        ["0", "1", "", "3"]
        (listSet 3 "3" ["0", "1"])

    , eq "Setting within list overwrites value"
        ["0", "1", "C", "3"]
        (listSet 2 "C" ["0", "1", "2", "3"])

    , eq "Setting at beginning and end overwrites"
        ["B", "1", "2", "E"]
        (listSet 3 "E" (listSet 0 "B" ["0", "1", "2", "3"]))
    ]


eq : String -> a -> a -> Test
eq desc act exp =
    test desc (\() -> Expect.equal act exp)
