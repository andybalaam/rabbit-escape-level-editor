module FlagsDecoderTests exposing (all)


import Json.Decode as D
import Test exposing (test, Test)
import Expect


import Flags exposing (..)
import FlagsDecoder exposing (flagsDecoder)
import Mode exposing (..)


all : Test
all = Test.concat
    [ test "Empty object produces defaults"
        (eq
            "{}"
            { worldText = "###\n# #\n###\n"
            , mode = View
            , urlPrefix = ""
            , id = ""
            }
        )

    , test "Provided fields are recognised"
        (eq
            ( "{\"worldText\": \"#r#\\n###\", " ++
              "\"mode\": \"Edit\", " ++
              "\"urlPrefix\": \"/x/\","++
              "\"id\": \"foo\""++
              "}"
            )
            { worldText = "#r#\n###"
            , mode = Edit
            , urlPrefix = "/x/"
            , id = "foo"
            }
        )

    , test "Bad JSON produces an Err"
        (assert_parsing_error
            "{\"worldText\": \"#r#\\n###\", \"m}"
        )
    ]


eq : String -> Flags -> () -> Expect.Expectation
eq input expected =
    \() ->
        Expect.equal
            (D.decodeString flagsDecoder input)
            (Ok expected)


assert_parsing_error : String -> () -> Expect.Expectation
assert_parsing_error input =
    \() ->
        case D.decodeString flagsDecoder input of
            Ok _ -> Expect.fail "Expected a parse error!"
            Err _ -> Expect.pass
