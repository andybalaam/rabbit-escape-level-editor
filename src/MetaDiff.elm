module MetaDiff exposing
    ( Diff
    , DiffValue
    , allOk
    , applyDiff
    , getDiff
    , emptyDiff
    , setDiff
    , toDiffList
    )


import Dict exposing (Dict)
import MetaLines exposing (MetaLines, SetFailed(..))
import MetaValue exposing (MetaValue(..))
import SimpleValue exposing (SimpleValue(..), simpleToMetaValue)


type alias DiffValue =
    { raw : String
    , parsed : Result SetFailed SimpleValue
    }


type alias Diff =
    Dict String DiffValue


emptyDiff : Diff
emptyDiff =
    Dict.empty


setDiff : String -> String -> Diff -> Diff
setDiff name value diff =
    let
        parseInt : String -> String -> Result SetFailed SimpleValue
        parseInt n v =
            case String.toInt v of
                Just i -> Ok (SvInt i)
                Nothing -> Err (BadValue n v)
    in
        case Dict.get name MetaLines.defaults of  -- Uses default, which feels bad?
            Just (MvString _) ->
                Dict.insert name {raw=value, parsed=Ok (SvString value)} diff
            Just (MvInt _) ->
                Dict.insert name {raw=value, parsed=parseInt name value} diff
            Just (MvList _) ->
                diff  -- TODO
            Nothing ->
                Debug.log ("setDiff: unknown name: " ++ name) diff


getDiff : String -> Diff -> Maybe DiffValue
getDiff =
    Dict.get


toDiffList : Diff -> List (String, DiffValue)
toDiffList diff =
    Dict.toList diff


-- Apply the supplied diff, ignoring any bad values
applyDiff : Diff -> MetaLines -> MetaLines
applyDiff diff metaLines =
    let
        setValue : String -> DiffValue -> MetaLines -> MetaLines
        setValue name diffValue mLs =
            case diffValue.parsed of
                Ok v -> Dict.insert name (simpleToMetaValue v) mLs
                Err _ -> mLs  -- Ignore errors
    in
        Dict.foldl setValue metaLines diff


allOk : Diff -> Bool
allOk diff =
    let
        parsedOk : (String, DiffValue) -> Bool
        parsedOk (_, val) =
            case val.parsed of
                Ok _ -> True
                Err _ -> False
    in
        List.all parsedOk (Dict.toList diff)
