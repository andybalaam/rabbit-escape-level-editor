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
import MetaLines exposing
    ( MetaLines
    , SetFailed(..)
    , Unwrapped
    , unwrap
    , withoutNumPart
    , wrap
    )
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
        default : Maybe MetaValue
        default =
            Dict.get (withoutNumPart name) MetaLines.defaults

        parseInt : String -> String -> Result SetFailed SimpleValue
        parseInt n v =
            case String.toInt v of
                Just i -> Ok (SvInt i)
                Nothing -> Err (BadValue n v)

        valueOfDefaultType : Result SetFailed SimpleValue
        valueOfDefaultType =
            case default of
                Just (MvString _) -> Ok (SvString value)
                Just (MvInt _) -> parseInt name value
                Just (MvList _) -> Ok (SvString value)
                Nothing ->
                    Debug.log
                        ("setDiff: unknown name: " ++ name)
                        (Err (UnknownName name))
    in
        Dict.insert name {raw=value, parsed=valueOfDefaultType} diff


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
        setValue : String -> DiffValue -> Unwrapped -> Unwrapped
        setValue name diffValue unwrapped =
            case diffValue.parsed of
                Ok v -> Dict.insert name v unwrapped
                Err _ -> unwrapped  -- Ignore errors
    in
        wrap (Dict.foldl setValue (unwrap metaLines) diff)


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
