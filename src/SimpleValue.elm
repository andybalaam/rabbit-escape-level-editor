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


-- metaValueToSimpleValues : MetaValue -> List SimpleValue
-- metaValueToSimpleValues metaValue =
--     case metaValue of
--         MvInt i -> [SvInt i]
--         MvString s -> [SvString s]
--         MvList ls -> List.map SvString ls
