module BoardTests exposing (..)

import Board exposing (Board, InsertionFailure(..), InsertionResult)
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, constant, int, intRange, list, string)
import MiniGoParser exposing (parse, parseWithDefault)
import Move
import Player exposing (Player, black, white)
import Position
import Set exposing (Set)
import Test exposing (..)


suite : Test
suite =
    describe "Board"
        [ describe "isPositionOnBoard"
            [ test "on board" <|
                \_ ->
                    Expect.ok (Board.isPositionOnBoard ( 3, 2 ) (Board.square 3))
            , test "off board" <|
                \_ ->
                    Expect.err (Board.isPositionOnBoard ( 0, 2 ) (Board.square 3))
            ]
        , describe "Board.capture"
            [ test "a capture" <|
                \_ ->
                    let
                        board =
                            parse """3
W--
B--
---
"""

                        stones =
                            Dict.fromList
                                [ ( ( 1, 2 ), Player.Black )
                                , ( ( 2, 1 ), Player.Black )
                                ]

                        captures =
                            ( [ ( 1, 1 ) ], [] )

                        expectedBoard =
                            Board.fromSizeStonesCaptures 3 stones captures
                    in
                    case board of
                        Err _ ->
                            Expect.fail "need a valid board"

                        Ok board_ ->
                            case Board.capture board_ (Move.fromPlayerAndPosition Player.Black ( 2, 1 )) of
                                Err _ ->
                                    Expect.fail "this should succeed"

                                Ok board__ ->
                                    Expect.equal board__ expectedBoard

            --  (Board {stones = expectedStones})
            -- Expect.ok (Board.isPositionOnBoard (3, 2) (Board.square 3))
            , test "not a capture" <|
                \_ ->
                    Expect.err (Board.isPositionOnBoard ( 0, 2 ) (Board.square 3))
            ]
        ]



-- groupTest : Test
-- groupTest =
--     describe "Group" [
--         test "groupAt emptyPosition" <|
--             \_ ->
--                 let
--                     expectedGroup = {
--                         positions= Set.empty
--                         , player= Nothing
--                         , liberties= Set.empty }
--                     board = tinyBoard
--                 in
--                 Expect.equal expectedGroup (Board.groupAt (1, 2) board)
--          test "groupAt group of on and three" <|
--             \_ ->
--                 let
--                     expectedGroup = {
--                         positions= Set.fromList [(2, 1)]
--                         , player= Just Player.white
--                         , liberties= Set.fromList [(3, 1), (2, 2), (1, 1)]  }
--                     board =  (Debug.log "grouptestbo9ard" (parseWithDefault """3\n-W-\n---\n---\n"""))
--                 in
--                 Expect.equal (Board.groupAt (2, 1) board) expectedGroup
--     ]
-- to do "Implement our first test. See https://package.elm-lang.org/packages/elm-explorations/test/latest for how to do this!"


tinyBoard : Board
tinyBoard =
    Board.square 3


boardWith : Int -> List ( Int, Int ) -> List ( Int, Int ) -> Board
boardWith size blacks whites =
    let
        fold player coordinates board =
            List.foldl
                (\pos board_ ->
                    Board.put (Move.fromPlayerAndPosition player pos) board_
                 -- /                            Result.Err    Debug.log "Error" ("Problem with " ++ Position.toString coordinate ++ " " ++ Player.toString player)
                )
                board
                coordinates
    in
    Board.square size |> fold black blacks |> fold white whites


inBounds : Int -> Fuzzer Int
inBounds size =
    intRange 1 size


outOfBounds : Int -> Fuzzer Int
outOfBounds size =
    Fuzz.oneOf
        [ constant 0
        , constant (size + 1)
        ]


fuzzPlayer : Fuzzer Player
fuzzPlayer =
    Fuzz.oneOf <| List.map constant [ white, black ]


insertionTest : String -> String -> ( Int, Int ) -> Player -> (InsertionResult -> Expectation) -> Test
insertionTest description boardString pos player expectation =
    test description <|
        \_ ->
            parse boardString
                |> Result.map (\board -> expectation <| Board.play (Move.fromPlayerAndPosition player pos) board)
                |> Result.withDefault (Expect.fail <| "parse error on " ++ boardString)


suicideTest : String -> String -> ( Int, Int ) -> Player -> Test
suicideTest description boardString coordinate player =
    insertionTest ("suicide: " ++ description)
        boardString
        coordinate
        player
        (\result ->
            case result of
                Ok _ ->
                    Expect.fail "should have been suicide"

                Err reason ->
                    Expect.equal reason Suicide
        )


validTest : String -> String -> ( Int, Int ) -> Player -> Test
validTest description boardString coordinate player =
    insertionTest ("valid: " ++ description)
        boardString
        coordinate
        player
        (\result ->
            case result of
                Ok board ->
                    case Board.get coordinate board of
                        Just player_ ->
                            Expect.equal player player_

                        Nothing ->
                            Expect.fail (Position.toString coordinate ++ " should have had " ++ Player.toString player ++ " instead of empty")

                Err reason ->
                    Expect.fail ("should have been valid instead of " ++ Debug.toString reason)
        )


moveTest : String -> String -> ( Int, Int ) -> Player -> String -> Test
moveTest description beforeBoard coordinate player afterBoard =
    insertionTest description
        beforeBoard
        coordinate
        player
        (\result ->
            case result of
                Ok board ->
                    Expect.equal afterBoard <| Board.toString board

                Err reason ->
                    Expect.fail ("should have been valid instead of " ++ Debug.toString reason)
        )



-- insertTests : Test
-- insertTests =
--     describe "Board.put"
--         [ fuzz2 (inBounds 3) (inBounds 3) "in an empty board" <|
--             \x y ->
--                 let
--                     coordinate =
--                         ( x, y )
--                     player =
--                         black
--                     result =
--                         Board.playMove coordinate player tinyBoard
--                 in
--                     case result of
--                         Ok board ->
--                             case Board.get coordinate board of
--                                 Nothing ->
--                                     Expect.fail "should be occupied"
--                                 Just insertedPlayer ->
--                                     Expect.equal insertedPlayer player
--                         Err _ ->
--                             Expect.fail "should have inserted successfully"
--         , fuzz2 (inBounds 3) (inBounds 3) "placing on an occupied coordinate" <|
--             \x y ->
--                 let
--                     coordinate =
--                         ( x, y )
--                     player =
--                         black
--                     board =
--                         case Board.playMove coordinate player tinyBoard of
--                             Ok b ->
--                                 b
--                             Err reason ->
--                                 tinyBoard
--                     result =
--                         Board.playMove coordinate player board
--                 in
--                     case result of
--                         Ok _ ->
--                             Expect.fail "should have failed"
--                         Err reason ->
--                             Expect.equal reason Occupied
--         , fuzz3 (outOfBounds 3) (outOfBounds 3) fuzzPlayer "fails on out of bounds" <|
--             \x y player ->
--                 let
--                     coordinate =
--                         ( x, y )
--                 in
--                     case Board.playMove coordinate player tinyBoard of
--                         Ok _ ->
--                             Expect.fail "should have out of bounds"
--                         Err reason ->
--                             Expect.equal reason OutOfBounds
--         , suicideTest "four opponent neighbors"
--             """3
-- -B-
-- B-B
-- -B-
-- """
--             ( 2, 2 )
--             white
--         , suicideTest "on edge"
--             """3
-- B--
-- -B-
-- B--
-- """
--             ( 1, 2 )
--             white
--         , validTest "with a single liberty left"
--             """3
-- B--
-- -B-
-- W--
-- """
--             ( 1, 2 )
--             white
--         , validTest "shared liberties"
--             """3
-- -BW
-- WBW
-- -B-
-- """
--             ( 1, 1 )
--             white
--         , moveTest "normal move"
--             """3
-- ---
-- B-B
-- -B-
-- """
--             ( 2, 2 )
--             white
--             """3
-- ---
-- BWB
-- -B-
-- """
--         , skip <|
--             moveTest "capture in the corner"
--                 """3
-- WB-
-- ---
-- ---
-- """
--                 ( 1, 2 )
--                 black
--                 """3
-- -B-
-- B--
-- ---
-- """
--         , skip <|
--             moveTest "capture with liberty race"
--                 """3
-- -BB
-- WWB
-- WWB
-- """
--                 ( 1, 1 )
--                 black
--                 """3
-- BBB
-- --B
-- --B
-- """
--         ]
-- toStringTests : Test
-- toStringTests =
--     describe "Board.toString"
--         [ test "an empty board" <|
--             \_ ->
--                 let
--                     board =
--                         Board.square 3
--                     expected =
--                         "3\n---\n---\n---\n"
--                 in
--                     Expect.equal expected <|     Board.toString board
--         , test "a board with some stuff" <|
--             \_ ->
--                 let
--                     board =
--                         boardWith 4 [ ( 1, 1 ) ] [ ( 2, 2 ) ]
--                     expected =
--                         "4\nB---\n-W--\n----\n----\n"
--                 in
--                     Expect.equal expected <| Board.toString board
--         ]
