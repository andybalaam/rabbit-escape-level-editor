module SimpleValue exposing (SimpleValue(..), simpleToMetaValue)

import MetaValue exposing (MetaValue(..))

type SimpleValue =
      SvInt Int
    | SvString String


simpleToMetaValue : SimpleValue -> MetaValue
simpleToMetaValue diffValue =
    case diffValue of
        SvInt i -> MvInt i
        SvString s -> MvString s
