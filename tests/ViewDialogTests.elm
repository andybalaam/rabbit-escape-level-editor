module ViewDialogTests exposing (all)


import Test exposing (describe,test,Test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, id, tag, text)
import Expect


import Dict exposing (Dict)
import Html exposing (Html, div)
import Html.Attributes exposing (for, type_, value)
import MetaLines
import Msg exposing (Msg)
import ViewDialog exposing (allMetaLineBoxes)


all : Test
all =
    describe "Tests of the dialog view"
        [ t "All defaults values are displayed"
            MetaLines.defaults
            MetaLines.emptyDiff
            [ check_label "name"
            , check_input "name" ""
            , check_label "description"
            , check_input "description" ""
            , check_label "author_name"
            , check_input "author_name" ""
            , check_label "author_url"
            , check_input "author_url" ""
            , check_label "hint.1"
            , check_input "hint.1" ""
            , check_label "hint.2"
            , check_input "hint.2" ""
            , check_label "hint.3"
            , check_input "hint.3" ""
            , check_label "hint.1.code"
            , check_input "hint.1.code" ""
            , check_label "hint.2.code"
            , check_input "hint.2.code" ""
            , check_label "hint.3.code"
            , check_input "hint.3.code" ""
            , check_label "solution.1"
            , check_input "solution.1" ""
            , check_label "solution.2"
            , check_input "solution.2" ""
            , check_label "solution.3"
            , check_input "solution.3" ""
            , check_label "solution.1.code"
            , check_input "solution.1.code" ""
            , check_label "solution.2.code"
            , check_input "solution.2.code" ""
            , check_label "solution.3.code"
            , check_input "solution.3.code" ""
            , check_label "num_rabbits"
            , check_input "num_rabbits" "10"
            , check_label "num_to_save"
            , check_input "num_to_save" "1"
            , check_label "rabbit_delay"
            , check_input "rabbit_delay" "4"
            , check_label "music"
            , check_input "music" ""
            , check_label "bash"
            , check_input "bash" "0"
            , check_label "dig"
            , check_input "dig" "0"
            , check_label "bridge"
            , check_input "bridge" "0"
            , check_label "block"
            , check_input "block" "0"
            , check_label "climb"
            , check_input "climb" "0"
            , check_label "explode"
            , check_input "explode" "0"
            , check_label "brolly"
            , check_input "brolly" "0"
            ]

--        , t "Multiple hints make multiple boxes"
--            (hintsOnly ["hint 1", "hint 2"])
--            MetaLines.emptyDiff
--            [ check_label "hint.1"
--            , check_input "hint.1" "hint 1"
--            , check_label "hint.2"
--            , check_input "hint.2" "hint 2"
--            ]
--
--        , t "A changed hint appears in its box"
--            (hintsOnly ["hint 1", "hint 2"])
--            (hintsDiff "hint.1" "changed 1")
--            [ check_label "hint.1"
--            , check_input "hint.1" "changed 1"
--            , check_label "hint.2"
--            , check_input "hint.2" "hint 2"
--            ]
--
--        , t "An added hint appears in its box"
--            (hintsOnly ["hint 1", "hint 2"])
--            (hintsDiff "hint.3" "new 3")
--            [ check_label "hint.1"
--            , check_input "hint.1" "hint 1"
--            , check_label "hint.2"
--            , check_input "hint.2" "hint 2"
--            , check_label "hint.3"
--            , check_input "hint.3" "new 3"
--            ]
        ]


-- hintsOnly : List String -> MetaLines.MetaLines
-- hintsOnly hints =
--     Dict.fromList [ ("hint", MetaLines.MvList hints) ]
--
--
-- hintsDiff : String -> String -> MetaLines.Diff
-- hintsDiff name value =
--     MetaLines.setDiff name value MetaLines.emptyDiff


wrap_in_div : List (Html Msg) -> Query.Single Msg
wrap_in_div items =
    div [] items |> Query.fromHtml


check_label : String -> Query.Single Msg -> (() -> Expect.Expectation)
check_label name boxes =
    \() ->
        boxes
            |> Query.find [ attribute (for ("id" ++ name)) ]
            |> Query.has [ text name, tag "label" ]


check_input : String -> String -> Query.Single Msg
    -> (() -> Expect.Expectation)
check_input name val boxes =
    \() ->
        boxes
            |> Query.find [ id ("id" ++ name) ]
            |> Query.has
                [ attribute (type_ "text")
                , attribute (value val)
                ]


t : String
  -> MetaLines.MetaLines
  -> MetaLines.Diff
  -> List (Query.Single Msg -> () -> Expect.Expectation)
  -> Test
t name starting_lines diff_to_apply expectations =

    let
        boxes : List (Html Msg)
        boxes =
            allMetaLineBoxes diff_to_apply starting_lines

        dv : Query.Single Msg
        dv =
            wrap_in_div boxes

        same_number : (() -> Expect.Expectation)
        same_number =
            \() -> Expect.equal (List.length boxes) (List.length expectations)

        exp : List (() -> Expect.Expectation)
        exp = List.map (\e -> e dv) expectations
    in
        test name <|
            Expect.all (exp ++ [same_number])
