module Position exposing
    ( Position
    , decoder
    , encode
    , fromXandY
    , isWithinSquare
    , toString
    , x
    , y
    )

import Json.Decode as D
import Json.Encode as E


type alias Position =
    ( Int, Int )


fromXandY : Int -> Int -> Position
fromXandY xx yy =
    ( xx, yy )


isWithinSquare : Int -> Position -> Bool
isWithinSquare size ( xx, yy ) =
    List.all identity
        (Debug.log ("isIn " ++ String.fromInt size ++ toString ( xx, yy ))
            [ xx > 0
            , xx <= size
            , yy > 0
            , yy <= size
            ]
        )


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
