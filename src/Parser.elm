module Parser exposing (parse)

import Regex exposing (HowMany(..))
import Board exposing (Board, set)
import Player exposing (white, black)


type alias ParseError =
    String


parse : String -> Result ParseError Board
parse input =
    let
        sizeResult =
            Regex.find (AtMost 1) (Regex.regex "^\\d+") input
                |> List.head
                |> Result.fromMaybe "leading digit not found"
                |> Result.andThen (String.toInt << .match)

        boardResult size =
            parseBoard (String.dropLeft (String.length <| toString size) input) size
    in
        sizeResult |> Result.andThen boardResult


parseBoard : String -> Int -> Result ParseError Board
parseBoard input size =
    parseRec size 0 input (Board.square size)


parseRec : Int -> Int -> String -> Board -> Result ParseError Board
parseRec size seen input board =
    case String.uncons input of
        Nothing ->
            if seen == size ^ 2 then
                Result.Ok board
            else
                Result.Err <| "Saw " ++ (toString seen) ++ " on a " ++ (toString size) ++ " board"

        Just ( c, rest ) ->
            let
                nextBoard player =
                    Board.set ( (seen % size) + 1, (seen // size) + 1 ) player board
                        |> parseRec size (seen + 1) rest
            in
                case c of
                    '\n' ->
                        parseRec size seen rest board

                    '-' ->
                        parseRec size (seen + 1) rest board

                    'W' ->
                        nextBoard white

                    'B' ->
                        nextBoard black

                    _ ->
                        Result.Err <| "Invalid char: " ++ (String.fromChar c)
