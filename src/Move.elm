module Move exposing
    ( Move
    , decoder
    , encode
    , fromPlayerAndPosition
    , fromPositionAndPlayer
    , player
    , position
    , toString
    )

import Json.Decode as D
import Json.Encode as E
import Player exposing (Player)
import Position exposing (Position)


type alias Move =
    ( Player, Position )


fromPlayerAndPosition : Player -> Position -> Move
fromPlayerAndPosition p coords =
    ( p, coords )


fromPositionAndPlayer : Position -> Player -> Move
fromPositionAndPlayer coords p =
    ( p, coords )


player : Move -> Player
player move =
    Tuple.first move


position : Move -> Position
position move =
    Tuple.second move


toString : Move -> String
toString move =
    Player.toString (player move) ++ Position.toString (position move)


encode : Move -> E.Value
encode m =
    E.object
        [ ( "player", Player.encode (player m) )
        , ( "position", Position.encode (position m) )
        ]


decoder : D.Decoder Move
decoder =
    D.map2 fromPlayerAndPosition
        (D.field "player" Player.decoder)
        (D.field "position" Position.decoder)
