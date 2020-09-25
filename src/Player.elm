module Player exposing
    ( Player(..)
    , black
    , decoder
    , encode
    , fromChar
    , fromSgf
    , next
    , toChar
    , toInt
    , toSgf
    , toString
    , white
    )

import Json.Decode as D
import Json.Encode as E
import Parser exposing ((|.), (|=), Parser)


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


fromChar : Char -> Maybe Player
fromChar c =
    case c of
        'W' ->
            Just White

        'B' ->
            Just Black

        _ ->
            Nothing


fromString : String -> Player
fromString c =
    case String.toLower c of
        "white" ->
            White

        "w" ->
            White

        "1" ->
            White

        "black" ->
            Black

        "b" ->
            Black

        "0" ->
            Black

        _ ->
            Black


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


encode : Player -> E.Value
encode p =
    E.string (toString p)


decoder : D.Decoder Player
decoder =
    D.map fromString D.string


fromSgf : Parser Player
fromSgf =
    Parser.oneOf
        [ Parser.map (\_ -> Black) (Parser.keyword "B")
        , Parser.map (\_ -> White) (Parser.keyword "W")
        ]


toSgf : Player -> String
toSgf p =
    case p of
        Black ->
            "B"

        White ->
            "W"



-- isNext: Player -> Bool
-- isNext other =
--     if other == (next
