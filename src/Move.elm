module Move
    exposing
        ( Move
        , fromPlayerAndCoordinate
        , player
        , position
        , toString
        )

import Player exposing (Player)
import Coordinate exposing (Coordinate)

type alias Move = (Player, Coordinate)

fromPlayerAndCoordinate : Player -> Coordinate -> Move
fromPlayerAndCoordinate p coords = (p, coords)


player : Move -> Player
player move = 
    Tuple.first move

position : Move -> Coordinate
position move = 
    Tuple.second move

toString : Move -> String
toString move = 
    Player.toString (player move) ++ Coordinate.toString (position move)