module GameTests exposing (..)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, constant, int, intRange, list, string)
import Game exposing (Game)
import Move exposing (Move)
import Parser exposing (Parser)
import Player exposing (Player(..))
import Set exposing (Set)
import Test exposing (..)


aGameWithMoves : Game
aGameWithMoves =
    let
        a =
            Game.fresh
    in
    { a | moves = [ Move.fromPlayerAndPosition Player.Black ( 1, 8 ) ] }


suite : Test
suite =
    describe "Game"
        [ describe "toSgf"
            [ test "fresh game" <|
                \_ ->
                    Expect.equal (Game.toSgf Game.fresh) ("(;" ++ String.concat (List.map Game.infoToSgf Game.fresh.infos) ++ ")")
            , test "Game with moves" <|
                \_ ->
                    let
                        sgf =
                            Game.toSgf aGameWithMoves

                        gameInfos =
                            String.concat (List.map Game.infoToSgf Game.fresh.infos)

                        moves =
                            String.concat (List.map Move.toSgf aGameWithMoves.moves)

                        expectedSgf =
                            "(;" ++ gameInfos ++ ";" ++ moves ++ ")"
                    in
                    Expect.equal sgf expectedSgf
            ]
        , describe "fromSgf"
            [ test "normal game" <|
                \_ ->
                    Expect.equal (Parser.run Game.fromSgf (Game.toSgf aGameWithMoves)) (Result.Ok aGameWithMoves)
            ]
        , describe "Game Arguments"
            [ test "can be parsed" <|
                \_ ->
                    Expect.equal (Parser.run Game.gameInfoParser "G[jo]") (Result.Ok <| Game.info "G" "jo")
            , test "can be parsed with two Letters" <|
                \_ ->
                    Expect.equal (Parser.run Game.gameInfoParser "GO[jo na]") (Result.Ok <| Game.info "GO" "jo na")
            ]
        ]
