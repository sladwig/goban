module Player exposing
    ( Player(..)
    , black
    , next
    , toChar
    , toInt
    , toString
    , white
    )


type Player
    = White
    | Black


black : Player
black =
    Black


white : Player
white =
    White


next : Player -> Player
next player =
    case player of
        White ->
            Black

        Black ->
            White


toChar : Player -> Char
toChar player =
    case player of
        White ->
            'W'

        Black ->
            'B'


toString : Player -> String
toString player =
    case player of
        Black ->
            "black"

        White ->
            "white"


toInt : Player -> Int
toInt player =
    case player of
        Black ->
            0

        White ->
            1



-- isNext: Player -> Bool
-- isNext other =
--     if other == (next
