module MetaLinesTests exposing (all)


import Dict
import Test exposing (test, Test)
import Expect


import MetaLines exposing (..)


all : Test
all = Test.concat
    [ assert_contains "Convert to list of string"
        ("num_rabbits", "10")
        (toStringList defaults)

    , assert_contains "Parse a correct single int value"
        ("num_to_save", "4")
        ( case parseAndSet "num_to_save" "4" defaults of
            Ok ml -> toStringList ml
            _ -> [("Failed to parse!", "")]
        )

    , eq "Convert to list of non-defaults is empty for default"
        []
        (toNonDefaultStringList defaults)

    , eq "Changed values appear in non-default list"
        ( Ok
            [ ("name", "Lev !")
            , ("num_to_save", "2")
            ]
        )
        ( parseAndSet "num_to_save" "2" defaults
            |> Result.andThen (parseAndSet "name" "Lev !")
            |> Result.map toNonDefaultStringList
        )

    , assert_contains "Parse a correct single string value"
        ("name", "My Level")
        ( case parseAndSet "name" "My Level" defaults of
            Ok ml -> toStringList ml
            _ -> [("Failed to parse!", "")]
        )

    , assert_contains "Default values stay after others are set"
        ("num_to_save", "1")
        ( case parseAndSet "num_rabbits" "34" defaults of
            Ok ml -> toStringList ml
            _ -> [("Failed to parse!", "")]
        )

    , eq "Setting an unknown value is an error"
        (parseAndSet "custom_field" "foo" defaults)
        (Err (UnknownName "custom_field"))

    , eq "Parsing a non-int into an int value is an error"
        (parseAndSet "num_rabbits" "foo" defaults)
        (Err (BadValue "num_rabbits" "foo"))

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
