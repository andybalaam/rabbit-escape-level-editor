module MetaValue exposing (MetaValue(..))


type MetaValue =
      MvInt Int
    | MvString String
    | MvList (List String)
