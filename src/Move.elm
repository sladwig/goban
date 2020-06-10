module Move exposing
    ( Move
    , fromPlayerAndPosition
    , fromPositionAndPlayer
    , player
    , position
    , toString
    )

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
