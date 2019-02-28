module SimpleValue exposing
    ( SimpleValue(..)
    , simpleToMetaValue
    , simpleValueToString
    )

import MetaValue exposing (MetaValue(..))

type SimpleValue =
      SvInt Int
    | SvString String


simpleToMetaValue : SimpleValue -> MetaValue
simpleToMetaValue diffValue =
    case diffValue of
        SvInt i -> MvInt i
        SvString s -> MvString s


simpleValueToString : SimpleValue -> String
simpleValueToString value =
    case value of
        SvInt i -> String.fromInt i
        SvString s -> s
