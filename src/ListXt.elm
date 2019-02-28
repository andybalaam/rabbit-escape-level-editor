module ListXt exposing (elemIndex, padToLength, set)


padToLength : Int -> List String -> List String
padToLength i lst =
    if i < 1 then
        lst
    else
        case lst of
            h :: t -> h :: padToLength (i-1) t
            [] -> "" :: padToLength (i-1) []


set : Int -> String -> List String -> List String
set i v lst =
    (padToLength i lst |> List.take i)
    ++ [v]
    ++ (List.drop (i+1) lst)


elemIndex : String -> List String -> Maybe Int
elemIndex item list =
    let
        impl : Int -> List String -> Maybe Int
        impl index remaining =
            case remaining of
                head :: tail ->
                    if head == item then
                        Just index
                    else
                        impl (index+1) tail
                _ ->
                    Nothing
    in
        impl 0 list
