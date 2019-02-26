module MetaLines exposing
    ( MetaLines
    , SetFailed(..)
    , Unwrapped
    , defaults
    , fromList
    , parseAndSet
    , toNonDefaultStringList
    , toStringList
    , unwrap
    )


import Dict exposing (Dict)
import MetaValue exposing (MetaValue(..))
import SimpleValue exposing (SimpleValue(..))


type alias MetaLines =
    Dict String MetaValue


defaultList : List (String, MetaValue)
defaultList =
    [ ("name", MvString "")
    , ("description", MvString "")
    , ("author_name", MvString "")
    , ("author_url", MvString "")
    , ("hint.1", MvString "")
    , ("hint.2", MvString "") -- TODO: expandable lists?
    , ("hint.3", MvString "")
    , ("hint.1.code", MvString "")
    , ("hint.2.code", MvString "")
    , ("hint.3.code", MvString "")
    , ("solution.1", MvString "")
    , ("solution.2", MvString "")
    , ("solution.3", MvString "")
    , ("solution.1.code", MvString "")
    , ("solution.2.code", MvString "")
    , ("solution.3.code", MvString "")
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


listToStringList : List (String, MetaValue) -> List (String, String)
listToStringList metaLines =
    let
        mvToString : (String, MetaValue) -> List (String, String)
        mvToString (name, value) =
            case value of
                MvInt i -> [(name, String.fromInt i)]
                MvString s -> [(name, s)]
                MvList ls ->
                    List.indexedMap
                        (\i s -> (name ++ "." ++ (String.fromInt (i + 1)), s))
                        ls
    in
        List.concatMap mvToString metaLines


toStringList : MetaLines -> List (String, String)
toStringList metaLines =
    let
        inOrder
            : List (String, MetaValue)
            -> MetaLines
            -> List (String, MetaValue)
        inOrder orderList mLs =
            case orderList of
                (name, _) :: ts ->
                    case Dict.get name mLs of
                        Just v -> (name, v) :: inOrder ts mLs
                        Nothing -> inOrder ts mLs
                _ ->
                    []
    in
        listToStringList (inOrder defaultList metaLines)


toNonDefaultStringList : MetaLines -> List (String, String)
toNonDefaultStringList metaLines =
    let
        nonDefault : String -> MetaValue -> Bool
        nonDefault name value =
            Dict.get name defaults /= Just value
    in
        listToStringList (Dict.toList (Dict.filter nonDefault metaLines))


type alias Unwrapped =
    Dict String SimpleValue


ithKey : String -> Int -> String
ithKey keyRoot i0 =
    keyRoot ++ "." ++ (String.fromInt (i0 + 1))


--wrap : Unwrapped -> MetaLines
--wrap metaLines =
--    todo
--
--
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
        setInt : String -> String -> MetaLines -> Result SetFailed MetaLines
        setInt n v mLs =
            case String.toInt value of
                Just i -> Ok (Dict.insert n (MvInt i) mLs)
                Nothing -> Err (BadValue n v)

        setString : String -> String -> MetaLines -> Result SetFailed MetaLines
        setString n v mLs =
            Ok (Dict.insert n (MvString v) mLs)
    in
        case Dict.get name metaLines of
            Just (MvInt _) ->
                setInt name value metaLines
            Just (MvString _) ->
                setString name value metaLines
            _ ->
                Err (UnknownName name)
--        Result.map wrap <|
--            case Dict.get name (unwrap metaLines) of
--                Just (MvInt _) ->
--                    setInt name value metaLines
--                Just (MvString _) ->
--                    setString name value metaLines
--                _ ->
--                    Err (UnknownName name)
