module MetaDiffTests exposing (all)


import Dict
import Test exposing (test, Test)
import Expect


import SimpleValue exposing (SimpleValue(..))
import MetaDiff exposing (..)
import MetaLines


all : Test
all = Test.concat
    [ eq "Applying an empty diff does nothing"
        (applyDiff emptyDiff MetaLines.defaults)
        MetaLines.defaults

    , eq "Convert diff to list"
        (toDiffList (setDiff "num_rabbits" "2" emptyDiff))
        [("num_rabbits", {raw="2", parsed=Ok (SvInt 2)})]

    , eq "Get a value from a diff"
        ( getDiff "num_rabbits"
            ( setDiff
                "num_rabbits"
                "2"
                (setDiff "num_to_save" "1" emptyDiff)
            )
        )
        (Just {raw="2", parsed=Ok (SvInt 2)})

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

    , eq "Lists can be built using diffs"
        [ ("hint.1", "foo1")
        , ("hint.2", "foo2")
        , ("hint.3", "")
        ]
        ( MetaLines.toNonDefaultStringList
            ( applyDiff
                ( setDiff
                    "hint.2"
                    "foo2"
                    (setDiff "hint.1" "foo1" emptyDiff)
                )
                MetaLines.defaults
            )
        )

    , eq "Lists with gaps can be built using diffs"
        [ ("hint.1", "")
        , ("hint.2", "foo2")
        , ("hint.3", "")
        , ("hint.4", "")
        , ("hint.5", "")
        , ("hint.6", "foo6")
        ]
        ( MetaLines.toNonDefaultStringList
            ( applyDiff
                ( setDiff
                    "hint.6"
                    "foo6"
                    (setDiff "hint.2" "foo2" emptyDiff)
                )
                MetaLines.defaults
            )
        )

    , eq "List diff items can be overwritten in the same diff"
        [ ("hint.1", "1")
        , ("hint.2", "B")
        , ("hint.3", "3")
        , ("hint.4", "D")
        ]
        ( MetaLines.toNonDefaultStringList
            ( applyDiff
                ( emptyDiff |>
                    setDiff "hint.1" "1" |>
                    setDiff "hint.2" "2" |>
                    setDiff "hint.3" "3" |>
                    setDiff "hint.4" "4" |>
                    setDiff "hint.2" "B" |>
                    setDiff "hint.4" "D"
                )
                MetaLines.defaults
            )
        )

    , eq "Lists items can be overwritten with a new diff"
        [ ("hint.1", "1")
        , ("hint.2", "B")
        , ("hint.3", "3")
        , ("hint.4", "D")
        ]
        ( MetaLines.toNonDefaultStringList
            (
                ( applyDiff
                    ( emptyDiff |>
                        setDiff "hint.1" "1" |>
                        setDiff "hint.2" "2" |>
                        setDiff "hint.3" "3" |>
                        setDiff "hint.4" "4"
                    )
                    MetaLines.defaults
                )
                |> ( applyDiff
                    ( emptyDiff |>
                        setDiff "hint.2" "B" |>
                        setDiff "hint.4" "D"
                    )
                )
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
