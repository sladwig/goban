module PositionTests exposing (..)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, constant, int, intRange, list, string)
import Parser
import Position
import Set exposing (Set)
import Test exposing (..)


suite : Test
suite =
    describe "Position"
        [ describe "fromSgf"
            [ test "correct" <|
                \_ ->
                    Expect.equal (Parser.run Position.fromSgf "ab")
                        (Result.Ok ( 1, 2 ))
            ]
        ]
