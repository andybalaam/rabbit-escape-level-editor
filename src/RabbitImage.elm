module RabbitImage exposing (rabbitImage)

import Rabbit exposing
    ( Direction(..)
    , Rabbit
    , RabbitType(..)
    , makeRabbit
    , makeRabbot
    )


rabbitImage : Rabbit -> String
rabbitImage rabbit =
    if rabbit.dir == Left then
        if rabbit.typ == Normal then
             "rabbit_stand_left.svg"
        else
             "rabbot_stand_left.svg"
    else
        if rabbit.typ == Normal then
             "rabbit_stand_right.svg"
        else
             "rabbot_stand_right.svg"
