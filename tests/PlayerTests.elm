module PlayerTests exposing (..)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, constant, int, intRange, list, string)
import Parser exposing (Parser)
import Player exposing (Player(..))
import Set exposing (Set)
import Test exposing (..)


suite : Test
suite =
    describe "Player"
        [ describe "fjromSgf"
            [ test "W" <|
                \_ ->
                    Expect.equal (Parser.run Player.fromSgf "W")
                        (Result.Ok Player.White)
            , test "B" <|
                \_ ->
                    Expect.equal (Parser.run Player.fromSgf "B") (Result.Ok Player.Black)
            ]
        ]
