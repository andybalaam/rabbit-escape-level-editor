module ThingImage exposing (thingImage)

import Thing exposing (Thing(..), TokenType(..))


thingImage : Thing -> String
thingImage thing =
    case thing of
        Entrance _ _ -> "entrance.png"
        Exit _ _ -> "exit.png"
        Fire _ _ -> "fire.png"
        Token Bash _ _    -> "token_bash.svg"
        Token Dig _ _     -> "token_dig.svg"
        Token Bridge _ _  -> "token_bridge.svg"
        Token BlockT _ _  -> "token_block.svg"
        Token Climb _ _   -> "token_climb.svg"
        Token Explode _ _ -> "token_explode.svg"
        Token Brolly _ _  -> "token_brolly.svg"
