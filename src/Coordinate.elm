module Coordinate
    exposing
        ( Coordinate
        , isWithinSquare
        , fromXandY
        )


type alias Coordinate =
    ( Int, Int )


{-| Make a Coordinate from an "x" and "y" value
-}
fromXandY : Int -> Int -> Coordinate
fromXandY x y=
    (x,y)


isWithinSquare : Int -> Coordinate -> Bool
isWithinSquare size ( x, y ) =
    List.all identity
        [ x > 0
        , x <= size
        , y > 0
        , y <= size
        ]
