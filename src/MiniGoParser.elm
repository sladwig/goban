module MiniGoParser exposing (parse, parseWithDefault)

import Board exposing (Board, put)
import Move
import Parser exposing ((|.), (|=), Parser, float, int, spaces, succeed, symbol)
import Player exposing (black, white)
import Regex


type alias ParseError =
    String


sizeParser : Parser Int
sizeParser =
    succeed identity
        |= int


parse : String -> Result ParseError Board
parse input =
    let
        -- sizeResult =
        --     Regex.find (AtMost 1) (Regex.regex "^\\d+") input
        --         |> List.head
        --         |> Result.fromMaybe "leading digit not found"
        --         |> Result.andThen (String.toInt << .match)
        sizeResult =
            case Parser.run int input of
                Err _ ->
                    Result.Err "we need a number first"

                Ok value ->
                    Result.Ok value

        -- |> List.head
        -- |> Result.fromMaybe "leading digit not found"
        -- |> Result.andThen (String.toInt << .match)
        boardResult size =
            parseBoard (String.dropLeft (String.length <| String.fromInt size) input) size
    in
    sizeResult |> Result.andThen boardResult


parseWithDefault : String -> Board
parseWithDefault input =
    case Debug.log "parsedefault" (parse input) of
        Err _ ->
            Board.square 1

        Ok board ->
            board


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
                Result.Err <| "Saw " ++ String.fromInt seen ++ " on a " ++ String.fromInt size ++ " board"

        Just ( c, rest ) ->
            let
                nextBoard player =
                    Board.put (Move.Normal player ( modBy size seen + 1, (seen // size) + 1 )) board
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
                    Result.Err <| "Invalid char: " ++ String.fromChar c
