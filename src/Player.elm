module Player
    exposing
        ( Player(..)
        , next
        , black
        , white
        , toChar
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
