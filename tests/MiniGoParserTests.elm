module MiniGoParserTests exposing (..)

import Test exposing (..)
import Expect
import MiniGoParser
import Board


suite : Test
suite =
    describe "Parser.parse integration" <|
        List.indexedMap
            (\index input ->
                test ("parse test " ++ String.fromInt index) <|
                    \_ ->
                        case MiniGoParser.parse input of
                            Err msg ->
                                Expect.fail msg

                            Ok parsed ->
                                Expect.equal input (Board.toString parsed)
            )
            [ "3\n---\n---\n---\n"
            , """3
W--
-BW
--B
"""
            , "0\n"
            , "1\nW\n"
            , """2
BW
WB
"""
            ]
