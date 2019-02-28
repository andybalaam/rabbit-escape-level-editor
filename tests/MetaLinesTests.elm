module MetaLinesTests exposing (all)


import Dict
import Test exposing (test, Test)
import Expect


import MetaLines exposing (..)
import MetaValue exposing (MetaValue(..))
import SimpleValue exposing (SimpleValue(..))


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

    , eq "Key without dot is equal to root key"
        "my_key"
        (upToDot "my_key")

    , eq "Root of key with dot is everything before dot"
        "key1x"
        (upToDot "key1x.2")

    , eq "Root of key with several dots is everything before first"
        "foo"
        (upToDot "foo.1.code")

    , eq "Key without dot has no number"
        ""
        (betweenFirst2Dots "my_key")

    , eq "Number of key with dot is after dot"
        "2"
        (betweenFirst2Dots "key1x.2")

    , eq "Number of key with multiple dots is second part"
        "20"
        (betweenFirst2Dots "foo.20.xxx")

    , eq "Convert to list of non-defaults is empty for default"
        []
        (toNonDefaultStringList defaults)

    , eq "List value is converted to numbered items"
        [ ("name.1", "v1")
        , ("name.2", "v2")
        ]
        ( toStringList (Dict.insert "name" (MvList ["v1", "v2"]) Dict.empty) )

    --, eq "Change existing list item"
    --    ( Ok
    --        [ ("name.1", "v1")
    --        , ("name.2", "changed")
    --        ]
    --    )
    --    ( Dict.fromList [("name", (MvList ["v1", "v2"]))]
    --        |> parseAndSet "name.2" "changed"
    --        |> Result.map toStringList
    --    )

    , eq "Wrapping empty list is empty metalines"
        Dict.empty
        ( wrap Dict.empty )

    , eq "Unwrapping empty metalines is empty list"
        Dict.empty
        ( unwrap Dict.empty )

    , eq "Wrapping normal values converts them simply"
        ( Dict.fromList
            [ ("a", MvInt 3)
            , ("b", MvInt 4)
            , ("c", MvString "v")
            , ("d", MvString "w")
            ]
        )
        ( Dict.fromList
            [ ("a", SvInt 3)
            , ("b", SvInt 4)
            , ("c", SvString "v")
            , ("d", SvString "w")
            ]
            |> wrap
        )

    , eq "Unwrapping normal values converts them simply"
        ( Dict.fromList
            [ ("a", SvInt 3)
            , ("b", SvInt 4)
            , ("c", SvString "v")
            , ("d", SvString "w")
            ]
        )
        ( Dict.fromList
            [ ("a", MvInt 3)
            , ("b", MvInt 4)
            , ("c", MvString "v")
            , ("d", MvString "w")
            ]
            |> unwrap
        )

    , eq "Wrapping list values builds a list"
        ( Dict.fromList
            [ ("k", MvList ["", "v", "", "w"])
            , ("yy", MvList ["11"])
            , ("yy.code", MvList ["**", "", "!!"])
            ]
        )
        ( Dict.fromList
            [ ("k.2", SvString "v")
            , ("k.4", SvString "w")
            , ("yy.1", SvString "11")
            , ("yy.3.code", SvString "!!")
            , ("yy.1.code", SvString "**")
            ]
            |> wrap
        )

    , eq "Unwrapping list values indexes keys"
        ( Dict.fromList
            [ ("k.1", SvString "v")
            , ("k.2", SvString "w")
            , ("y.1", SvString "11")
            , ("yy.1.code", SvString "**")
            , ("yy.2.code", SvString "")
            , ("yy.3.code", SvString "!!")
            ]
        )
        ( Dict.fromList
            [ ("k", MvList ["v", "w"])
            , ("x", MvList [])
            , ("yy.code", MvList ["**", "", "!!"])
            , ("y", MvList ["11"])
            ]
            |> unwrap
        )

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
