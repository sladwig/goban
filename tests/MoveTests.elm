module MoveTests exposing (..)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, constant, int, intRange, list, string)
import Move exposing (Move(..))
import Parser exposing (Parser)
import Player exposing (Player(..))
import Set exposing (Set)
import Test exposing (..)
import Time


suite : Test
suite =
    describe "Move"
        [ describe "fromSgf"
            [ test "normal" <|
                \_ ->
                    Expect.equal (Parser.run Move.fromSgf "B[af]")
                        (Result.Ok (Move.fromPlayerAndPosition Player.Black ( 1, 6 )))
            , test "timed" <|
                \_ ->
                    Expect.equal (Parser.run Move.fromSgf "W[fa]DT[123]")
                        (Result.Ok (Move.fromPlayerAndPositionAndTime Player.White ( 6, 1 ) (Time.millisToPosix 123)))
            ]
        ]
