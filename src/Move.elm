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
    = NormalMove Player Position
    | TimedMove Player Position Time.Posix





player : Move -> Player
player move =
    case move of
        TimedMove p _ _ ->
            p

        NormalMove p _ ->
            p


position : Move -> Position
position move =
    case move of
        TimedMove _ pos _ ->
            pos

        NormalMove _ pos ->
            pos


toString : Move -> String
toString move =
    case move of
        TimedMove p pos t ->
            Player.toString p ++ Position.toString pos ++ "@" ++ String.fromInt (Time.posixToMillis t)

        NormalMove p pos ->
            Player.toString p ++ Position.toString pos


encode : Move -> E.Value
encode m =
    case m of
        TimedMove p pos t ->
            E.object
                [ ( "player", Player.encode p )
                , ( "position", Position.encode pos )
                , ( "at", E.int (Time.posixToMillis t) )
                ]

        NormalMove p pos ->
            E.object
                [ ( "player", Player.encode p )
                , ( "position", Position.encode pos )
                ]


decoder : D.Decoder Move
decoder =
    D.oneOf
        [ D.map3 TimedMove
            (D.field "player" Player.decoder)
            (D.field "position" Position.decoder)
            (D.map Time.millisToPosix (D.field "at" D.int))
        , D.map2 NormalMove
            (D.field "player" Player.decoder)
            (D.field "position" Position.decoder)
        ]
