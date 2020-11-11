module Move exposing
    ( Move(..)
    , decoder
    , encode
    , fromPlayerAndPosition
    , fromPlayerAndPositionAndTime
    , fromSgf
    , playerOf
    , positionOf
    , toSgf
    , toString
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as E
import Parser exposing ((|.), (|=), Parser)
import Player exposing (Player)
import Position exposing (Position)
import Time


type Move
    = Timed
        { player : Player
        , position : Position
        , at : Time.Posix
        }
    | Normal
        { player : Player
        , position : Position
        }


playerOf : Move -> Player
playerOf move =
    case move of
        Timed { player } ->
            player

        Normal { player } ->
            player


positionOf : Move -> Position
positionOf move =
    case move of
        Timed { position } ->
            position

        Normal { position } ->
            position


toString : Move -> String
toString move =
    case move of
        Timed m ->
            Player.toString m.player ++ Position.toString m.position ++ "@" ++ String.fromInt (Time.posixToMillis m.at)

        Normal m ->
            Player.toString m.player ++ Position.toString m.position


encode : Move -> E.Value
encode move =
    case move of
        Timed m ->
            E.object
                [ ( "player", Player.encode m.player )
                , ( "position", Position.encode m.position )
                , ( "at", E.int (Time.posixToMillis m.at) )
                ]

        Normal m ->
            E.object
                [ ( "player", Player.encode m.player )
                , ( "position", Position.encode m.position )
                ]


fromPlayerAndPosition : Player -> Position -> Move
fromPlayerAndPosition player position =
    Normal { player = player, position = position }


fromPlayerAndPositionAndTime : Player -> Position -> Time.Posix -> Move
fromPlayerAndPositionAndTime player position at =
    Timed { player = player, position = position, at = at }


decoder : Decoder Move
decoder =
    Decode.oneOf
        [ Decode.map3 fromPlayerAndPositionAndTime
            (Decode.field "player" Player.decoder)
            (Decode.field "position" Position.decoder)
            (Decode.map Time.millisToPosix (Decode.field "at" Decode.int))
        , Decode.map2 fromPlayerAndPosition
            (Decode.field "player" Player.decoder)
            (Decode.field "position" Position.decoder)
        ]


toSgf : Move -> String
toSgf move =
    case move of
        Timed m ->
            Player.toSgf (.player m) ++ "[" ++ Position.toSgf (.position m) ++ "]DT[" ++ String.fromInt (Time.posixToMillis m.at) ++ "]"

        Normal m ->
            Player.toSgf (.player m) ++ "[" ++ Position.toSgf (.position m) ++ "]"


timefromSgf : Parser Time.Posix
timefromSgf =
    Parser.succeed Time.millisToPosix
        |= Parser.int


fromParsing : Player -> Position -> Maybe Time.Posix -> Move
fromParsing p pos a =
    case a of
        Just at ->
            fromPlayerAndPositionAndTime p pos at

        Nothing ->
            fromPlayerAndPosition p pos


fromSgf : Parser Move
fromSgf =
    Parser.succeed fromParsing
        |= Player.fromSgf
        |. Parser.symbol "["
        |= Position.fromSgf
        |. Parser.symbol "]"
        |= Parser.oneOf
            [ Parser.succeed Just
                |. Parser.token "DT"
                |. Parser.symbol "["
                |= timefromSgf
                |. Parser.symbol "]"
            , Parser.succeed Nothing
            ]
