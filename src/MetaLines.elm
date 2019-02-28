module MetaLines exposing
    ( MetaLines
    , SetFailed(..)
    , Unwrapped
    , betweenFirst2Dots
    , defaults
    , fromList
    , parseAndSet
    , toNonDefaultStringList
    , toStringList
    , unwrap
    , upToDot
    , wrap
    )


import Dict exposing (Dict)
import ListXt
import MetaValue exposing (MetaValue(..))
import SimpleValue exposing (SimpleValue(..), simpleToMetaValue)


type alias MetaLines =
    Dict String MetaValue


defaultList : List (String, MetaValue)
defaultList =
    [ ("name", MvString "")
    , ("description", MvString "")
    , ("author_name", MvString "")
    , ("author_url", MvString "")
    , ("hint", MvList ["", "", ""])
    , ("hint.code", MvList ["", "", ""])
    , ("solution", MvList ["", "", ""])
    , ("solution.code", MvList ["", "", ""])
    , ("num_rabbits", MvInt 10)
    , ("num_to_save", MvInt 1)
    , ("rabbit_delay", MvString "4") -- TODO: list of ints
    , ("music", MvString "")
    , ("bash", MvInt 0)
    , ("dig", MvInt 0)
    , ("bridge", MvInt 0)
    , ("block", MvInt 0)
    , ("climb", MvInt 0)
    , ("explode", MvInt 0)
    , ("brolly", MvInt 0)
    ]


defaults : MetaLines
defaults =
    Dict.fromList defaultList


fromList : List (String, MetaValue) -> MetaLines
fromList values =
    let
        -- Ignore any bad keys
        checkedSet : (String, MetaValue) -> MetaLines -> MetaLines
        checkedSet (name, value) existing =
            case Dict.get name defaults of
                Just _ -> Dict.insert name value existing
                Nothing -> Debug.log ("fromList: Bad name! " ++ name) existing
    in
        List.foldl checkedSet defaults values


toStringList : MetaLines -> List (String, String)
toStringList metaLines =
    let
        find : Int -> String -> List String -> Int
        find i item list =
            case list of
                h :: t ->
                    if h == item then i else find (i+1) item t
                _ ->
                    -1

        orderOf
            : List String
            -> (String, SimpleValue)
            -> (String, SimpleValue)
            -> Order
        orderOf base (a, _) (b, _) =
            let
                ia = find 0 (upToDot a) base
                ib = find 0 (upToDot b) base
            in
                compare ia ib

        simpleValueToString : SimpleValue -> String
        simpleValueToString value =
            case value of
                SvInt i -> String.fromInt i
                SvString s -> s

    in
        metaLines
            |> unwrap
            |> Dict.toList
            |> List.sortWith (orderOf (List.map Tuple.first defaultList))
            |> List.map (\(k, v) -> (k, simpleValueToString v))


toNonDefaultStringList : MetaLines -> List (String, String)
toNonDefaultStringList metaLines =
    let
        nonDefault : String -> MetaValue -> Bool
        nonDefault name value =
            Dict.get name defaults /= Just value
    in
        metaLines
            |> Dict.filter nonDefault
            |> toStringList


type alias Unwrapped =
    Dict String SimpleValue


ithKey : String -> Int -> String
ithKey keyWithoutNum i0 =
    let
        root = upToDot keyWithoutNum
        num = String.fromInt (i0 + 1)
        rest = List.drop 1 (String.split "." keyWithoutNum)
    in
        String.join "." (root :: num :: rest)


upToDot : String -> String
upToDot fullName =
    String.split "." fullName |>
        List.head |>
        Maybe.withDefault ""


betweenFirst2Dots : String -> String
betweenFirst2Dots fullName =
    String.split "." fullName |>
        List.drop 1 |>
        List.head |>
        Maybe.withDefault ""


fromSecondDot : String -> String
fromSecondDot fullName =
    String.split "." fullName |>
        List.drop 2 |>
        String.join "."


mergeIntoList : Int -> SimpleValue -> Maybe MetaValue -> Maybe MetaValue
mergeIntoList i0 new_value old_value =
    let
        old_list : List String
        old_list =
            case old_value of
                Just (MvList ls) -> ls
                _ -> []
                -- anything that is missing or not a list becomes []

        new_string : String
        new_string =
            case new_value of
                SvString s -> s
                _ -> "Error: non-string in list!"
    in
        Just <| MvList <| ListXt.listSet i0 new_string old_list


updateMetaValue
    : Maybe Int -> SimpleValue -> Maybe MetaValue -> Maybe MetaValue
updateMetaValue list_index1 new_value old_value =
    case list_index1 of
        Nothing ->
            -- This did not come from a numbered key:
            -- replace what was there.
            Just (simpleToMetaValue new_value)

        Just i1 ->
            -- This came from a numbered key:
            -- merge it into the existing list
            mergeIntoList (i1 - 1) new_value old_value


withoutNumPart : String -> String
withoutNumPart key =
    case fromSecondDot key of
        "" -> upToDot key
        x -> upToDot key ++ "." ++ x


insertByKey : String -> SimpleValue -> MetaLines -> MetaLines
insertByKey key value metaValues =
    let
        kNum : Maybe Int
        kNum = String.toInt (betweenFirst2Dots key)
    in
        Dict.update
            (withoutNumPart key)
            (updateMetaValue kNum value)
            metaValues


wrap : Unwrapped -> MetaLines
wrap unwrapped =
    Dict.foldl insertByKey Dict.empty unwrapped


unwrap : MetaLines -> Unwrapped
unwrap metaLines =
    metaLines
        |> Dict.toList
        |> List.concatMap
            ( \(k, v) ->
                case v of
                    MvInt i -> [(k, SvInt i)]
                    MvString s -> [(k, SvString s)]
                    MvList ls ->
                        List.indexedMap
                            (\i lv -> (ithKey k i, SvString lv))
                            ls
            )
        |> Dict.fromList


type SetFailed =
      UnknownName String
    | BadValue String String


parseAndSet : String -> String -> MetaLines -> Result SetFailed MetaLines
parseAndSet name value metaLines =
    let
        setInt : String -> String -> Unwrapped -> Result SetFailed Unwrapped
        setInt n v mLs =
            case String.toInt value of
                Just i -> Ok (Dict.insert n (SvInt i) mLs)
                Nothing -> Err (BadValue n v)

        setString : String -> String -> Unwrapped -> Result SetFailed Unwrapped
        setString n v mLs =
            Ok (Dict.insert n (SvString v) mLs)

        unwrapped : Unwrapped
        unwrapped =
            unwrap metaLines
    in
        Result.map wrap <|
            case Dict.get name unwrapped of
                Just (SvInt _) ->
                    setInt name value unwrapped
                Just (SvString _) ->
                    setString name value unwrapped
                _ ->
                    Err (UnknownName name)
