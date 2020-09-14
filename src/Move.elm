module Move exposing
    ( Move(..)
    , decoder
    , encode
    , player
    , position
    , toString
    )

import Json.Decode as D
import Json.Encode as E
import Player exposing (Player)
import Position exposing (Position)
import Time


type Move
    = Normal Player Position
    | Timed Player Position Time.Posix


player : Move -> Player
player move =
    case move of
        Timed p _ _ ->
            p

        Normal p _ ->
            p


position : Move -> Position
position move =
    case move of
        Timed _ pos _ ->
            pos

        Normal _ pos ->
            pos


toString : Move -> String
toString move =
    case move of
        Timed p pos t ->
            Player.toString p ++ Position.toString pos ++ "@" ++ String.fromInt (Time.posixToMillis t)

        Normal p pos ->
            Player.toString p ++ Position.toString pos


encode : Move -> E.Value
encode m =
    case m of
        Timed p pos t ->
            E.object
                [ ( "player", Player.encode p )
                , ( "position", Position.encode pos )
                , ( "at", E.int (Time.posixToMillis t) )
                ]

        Normal p pos ->
            E.object
                [ ( "player", Player.encode p )
                , ( "position", Position.encode pos )
                ]


decoder : D.Decoder Move
decoder =
    D.oneOf
        [ D.map3 Timed
            (D.field "player" Player.decoder)
            (D.field "position" Position.decoder)
            (D.map Time.millisToPosix (D.field "at" D.int))
        , D.map2 Normal
            (D.field "player" Player.decoder)
            (D.field "position" Position.decoder)
        ]
