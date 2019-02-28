module ListXt exposing (padToLength, set)


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
