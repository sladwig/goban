module Game exposing (Game, fresh, fromSgf, gameInfoParser, info, infoToSgf, toSgf)

import Debug
import Move exposing (Move)
import Parser exposing ((|.), (|=), Parser)


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
