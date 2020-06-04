module Coordinate exposing
    ( Coordinate
    , fromXandY
    , isWithinSquare
    , toString
    , x
    , y
    )


type alias Coordinate =
    ( Int, Int )


fromXandY : Int -> Int -> Coordinate
fromXandY xx yy =
    ( xx, yy )


isWithinSquare : Int -> Coordinate -> Bool
isWithinSquare size ( xx, yy ) =
    List.all identity
        [ xx > 0
        , xx <= size
        , yy > 0
        , yy <= size
        ]


x : Coordinate -> Int
x coords =
    Tuple.first coords


y : Coordinate -> Int
y coords =
    Tuple.second coords


toString : Coordinate -> String
toString coords =
    "(" ++ String.fromInt (Tuple.first coords) ++ "|" ++ String.fromInt (Tuple.second coords) ++ ")"
