module SimpleValueTests exposing (all)


import Dict
import Test exposing (test, Test)
import Expect


import MetaValue exposing (..)
import SimpleValue exposing (..)


all : Test
all = Test.concat
    [ eq "Simple String to Meta String"
        (MvString "goo")
        (simpleToMetaValue (SvString "goo"))

    , eq "Simple Int to Meta Int"
        (MvInt 45)
        (simpleToMetaValue (SvInt 45))

    , eq "Simple String to String"
        "bar"
        (simpleValueToString (SvString "bar"))

    , eq "Simple Int to String"
        "76"
        (simpleValueToString (SvInt 76))
    ]


eq : String -> a -> a -> Test
eq desc act exp =
    test desc (\() -> Expect.equal act exp)
