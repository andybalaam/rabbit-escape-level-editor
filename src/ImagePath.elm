module ImagePath exposing (imagePath)


import Flags exposing (Flags)


imagePath : Flags -> String -> String
imagePath flags fileName =
    flags.urlPrefix ++ "images/" ++ fileName


