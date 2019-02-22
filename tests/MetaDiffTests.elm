module MetaDiffTests exposing (all)


import Dict
import Test exposing (test, Test)
import Expect


import MetaDiff exposing (..)
import MetaLines


all : Test
all = Test.concat
    [ eq "Applying an empty diff does nothing"
        (applyDiff emptyDiff MetaLines.defaults)
        MetaLines.defaults

    , eq "Convert diff to list"
        (toDiffList (setDiff "num_rabbits" "2" emptyDiff))
        [("num_rabbits", {raw="2", parsed=Ok (MetaLines.MvInt 2)})]

    , eq "Get a value from a diff"
        ( getDiff "num_rabbits"
            ( setDiff
                "num_rabbits"
                "2"
                (setDiff "num_to_save" "1" emptyDiff)
            )
        )
        (Just {raw="2", parsed=Ok (MetaLines.MvInt 2)})

    , eq "Get a missing value from a diff"
        ( getDiff "num_rabbits"
            ( setDiff "num_to_save" "1" emptyDiff)
        )
        Nothing

    , eq "A valid diff is all OK"
        ( allOk
            ( setDiff
                "num_rabbits"
                "2"
                (setDiff "num_to_save" "1" emptyDiff)
            )
        )
        True

    , eq "An invalid diff is not all OK"
        ( allOk
            ( setDiff
                "num_rabbits"
                "2"
                (setDiff "num_to_save" "all" emptyDiff)
            )
        )
        False

    , assert_contains "Applying a valid diff updates the lines"
        ("num_rabbits", "3")
        ( MetaLines.toStringList
            (applyDiff
                (setDiff "num_rabbits" "3" emptyDiff)
                MetaLines.defaults
            )
        )

    , assert_contains "Applying an invalid diff makes no change"
        ("num_rabbits", "10")
        ( MetaLines.toStringList
            (applyDiff
                (setDiff "num_rabbits" "BAD_INT" emptyDiff)
                MetaLines.defaults
            )
        )

    , assert_contains "Applying valid and invalid values applies the valid"
        ("num_rabbits", "3")
        ( MetaLines.toStringList
            (applyDiff
                (setDiff "num_rabbits" "3"
                    (setDiff "num_to_save" "BAD"
                        (setDiff "name" "Ma Lev" emptyDiff)
                    )
                )
                MetaLines.defaults
            )
        )
    ]


listContains : a -> List a -> Expect.Expectation
listContains item list =
    if List.member item list then
        Expect.pass
    else
        Expect.fail
            (  "listContains: the list: "
            ++ Debug.toString list
            ++ " does not contain: "
            ++ Debug.toString item
            ++ "."
            )


eq : String -> a -> a -> Test
eq desc act exp =
    test desc (\() -> Expect.equal act exp)

assert_contains : String -> a -> List a -> Test
assert_contains desc item list =
    test desc (\() -> listContains item list)
