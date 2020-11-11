module Game exposing (Game, decoder, encode, fresh, fromMoves, fromSgf, gameInfoParser, info, infoToSgf, makeMove, toBoard, toSgf, toTurn, undoMove)

import Board exposing (Board)
import Debug
import Json.Decode as D
import Json.Encode as E
import List.Extra as ListExtra
import Move exposing (Move)
import Parser exposing ((|.), (|=), Parser)
import Player exposing (Player)


type alias Game =
    { infos : List GameInfo, moves : List Move }


fresh : Game
fresh =
    { infos =
        [ info "G" "1"
        , info "FF" "4"
        , info "CA" "UTF-8"
        , info "KM" "0"
        , info "SZ" "19"
        , info "DT" "_the time_"
        , info "BP" "_player black_"
        , info "BR" "_black rank_"
        , info "WP" "_player white_"
        , info "WR" "_white rank_"
        , info "GN" "_Game Name_"
        , info "GC" "_Game Comment_"
        , info "AP" "GOBAN 1.0"
        ]
    , moves = []
    }


withInfos : Game -> List GameInfo -> Game
withInfos g i =
    { g | infos = i }


toSgf : Game -> String
toSgf g =
    let
        attrs =
            String.concat (List.map infoToSgf g.infos)

        moveList =
            String.join ";" (List.map Move.toSgf g.moves)

        madeMoves =
            if 0 == String.length moveList then
                ""

            else
                ";" ++ moveList
    in
    "(;" ++ attrs ++ madeMoves ++ ")"


fromParsing : List GameInfo -> List Move -> Game
fromParsing i m =
    { fresh | infos = i, moves = m }


fromSgf : Parser Game
fromSgf =
    Parser.succeed fromParsing
        |. Parser.symbol "("
        |. Parser.symbol ";"
        |= gameInfosParser
        |= movesParser
        |. Parser.symbol ")"


type alias GameInfo =
    { attribute : String, value : String }


info : String -> String -> GameInfo
info a v =
    { attribute = a, value = v }


gameInfoParser : Parser GameInfo
gameInfoParser =
    Parser.succeed info
        |= (Parser.getChompedString <|
                Parser.succeed ()
                    |. Parser.chompWhile Char.isUpper
           )
        |. Parser.symbol "["
        |= (Parser.getChompedString <|
                Parser.succeed ()
                    |. Parser.chompWhile (\c -> c /= ']')
           )
        |. Parser.symbol "]"


gameInfosParser : Parser (List GameInfo)
gameInfosParser =
    Parser.loop [] gameInfosParserHelper


gameInfosParserHelper : List GameInfo -> Parser (Parser.Step (List GameInfo) (List GameInfo))
gameInfosParserHelper gameInfos =
    Parser.oneOf
        [ Parser.succeed (\i -> Parser.Loop (i :: gameInfos))
            |= gameInfoParser
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse gameInfos))
        ]


movesParser : Parser (List Move)
movesParser =
    Parser.loop [] movesParserHelper


movesParserHelper : List Move -> Parser (Parser.Step (List Move) (List Move))
movesParserHelper gameMoves =
    Parser.oneOf
        [ Parser.succeed (\m -> Parser.Loop (m :: gameMoves))
            |. Parser.symbol ";"
            |= Move.fromSgf
        , Parser.succeed ()
            |> Parser.map (\_ -> Parser.Done (List.reverse gameMoves))
        ]


infoToSgf : GameInfo -> String
infoToSgf a =
    a.attribute ++ "[" ++ a.value ++ "]"


fromMoves : List Move -> Game
fromMoves moves =
    { fresh | moves = moves }


toBoard : List Move -> Board
toBoard moves =
    List.foldl Board.applyPlay (Board.square 19) moves


toTurn : List Move -> Player
toTurn moves =
    case modBy 2 (List.length moves) of
        0 ->
            Player.black

        _ ->
            Player.white


makeMove : Game -> Move -> Game
makeMove game move =
    { game | moves = game.moves ++ [ move ] }


undoMove : Game -> Game
undoMove game =
    let
        oneLessMove =
            case ListExtra.init game.moves of
                Just someMoves ->
                    someMoves

                Nothing ->
                    []
    in
    { game | moves = oneLessMove }



-- JSON ENCODE/DECODE


encode : Game -> E.Value
encode game =
    E.object
        [ ( "moves", E.list Move.encode game.moves )
        ]


decoder : D.Decoder Game
decoder =
    D.map
        fromMoves
        (D.field "moves" (D.list Move.decoder))
