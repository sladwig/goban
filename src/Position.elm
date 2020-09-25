module Position exposing
    ( Position
    , decoder
    , encode
    , fromSgf
    , fromXandY
    , isWithinSquare
    , toSgf
    , toString
    , x
    , y
    )

import Array
import Json.Decode as D
import Json.Encode as E
import List.Extra as ListExtra
import Parser exposing ((|.), (|=), Parser)


type alias Position =
    ( Int, Int )


fromXandY : Int -> Int -> Position
fromXandY xx yy =
    ( xx, yy )


isWithinSquare : Int -> Position -> Bool
isWithinSquare size ( xx, yy ) =
    List.all identity
        [ xx > 0
        , xx <= size
        , yy > 0
        , yy <= size
        ]


x : Position -> Int
x coords =
    Tuple.first coords


y : Position -> Int
y coords =
    Tuple.second coords


toString : Position -> String
toString coords =
    "(" ++ String.fromInt (Tuple.first coords) ++ "|" ++ String.fromInt (Tuple.second coords) ++ ")"


encode : Position -> E.Value
encode pos =
    E.object
        [ ( "x", E.int (x pos) )
        , ( "y", E.int (y pos) )
        ]


decoder : D.Decoder Position
decoder =
    D.map2 fromXandY
        (D.field "x" D.int)
        (D.field "y" D.int)


toSgf : Position -> String
toSgf pos =
    toAlphabet (x pos) ++ toAlphabet (y pos)



--positionParser :


fromSgf : Parser Position
fromSgf =
    Parser.succeed fromXandY
        |= sgfPositionChar
        |= sgfPositionChar


sgfPositionChar : Parser Int
sgfPositionChar =
    Parser.succeed toInt
        |= (Parser.getChompedString <|
                Parser.succeed ()
                    |. Parser.chompIf Char.isLower
           )


abc =
    [ "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z" ]


toAlphabet : Int -> String
toAlphabet i =
    let
        alphabet =
            Array.fromList abc
    in
    case Array.get (i - 1) alphabet of
        Just letter ->
            letter

        Nothing ->
            ""


toInt : String -> Int
toInt str =
    let
        index =
            ListExtra.elemIndex str abc
    in
    case index of
        Just n ->
            n + 1

        Nothing ->
            0
