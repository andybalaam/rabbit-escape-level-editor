module ListXt exposing (listPadToLength, listSet)


listPadToLength : Int -> List String -> List String
listPadToLength i lst =
    if i < 1 then
        lst
    else
        case lst of
            h :: t -> h :: listPadToLength (i-1) t
            [] -> "" :: listPadToLength (i-1) []


listSet : Int -> String -> List String -> List String
listSet i v lst =
    (listPadToLength i lst |> List.take i)
    ++ [v]
    ++ (List.drop (i+1) lst)
