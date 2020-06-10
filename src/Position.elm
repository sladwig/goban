module Position exposing
    ( Position
    , fromXandY
    , isWithinSquare
    , toString
    , x
    , y
    )


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
