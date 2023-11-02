module Game exposing (Game, Sgf, fresh, fromSgf, gameInfoParser, info, infoToSgf, makeMove, toBoard, toSgf, toString, toTurn, undoMove)

-- import Debug
-- import Json.Decode as D
-- import Json.Encode as E

import Board exposing (Board, BoardSized(..))
import List.Extra as ListExtra
import Move exposing (Move)
import Parser exposing ((|.), (|=), Parser)
import Player exposing (Player)


type alias Game =
    { infos : GameInfos, moves : List Move, bs : BoardSized }



-- type alias Sgf =
--     String


type Sgf
    = Sgf String


fresh : BoardSized -> Game
fresh boardSize =
    { infos =
        [ info "G" "1"
        , info "FF" "4"
        , info "CA" "UTF-8"
        , info "KM" "0"
        , info "SZ" (String.fromInt (Board.bs2Int (Debug.log "bbbbs" boardSize)))
        , info "DT" "_the time_"
        , info "BP" "_player black_"
        , info "BR" "_black rank_"
        , info "WP" "_player white_"
        , info "WR" "_white rank_"
        , info "GN" "_Game Name_"
        , info "GC" "_Game Comment_"
        , info "AP" "GOBAN 1.1"
        ]
    , moves = []
    , bs = boardSize
    }


withInfos : Game -> GameInfos -> Game
withInfos g i =
    { g | infos = i }


toSgf : Game -> Sgf
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
    Sgf ("(;" ++ attrs ++ madeMoves ++ ")")


toString : Sgf -> String
toString (Sgf sgf) =
    sgf


readFromGameInfos : GameInfos -> String -> Maybe String
readFromGameInfos infos attr =
    case Debug.log "ja nuuuu: " (List.filter (\i -> i.attribute == attr) infos) of
        [ { value } ] ->
            Just value

        _ ->
            Nothing



-- Debug.crash ("readFromGameInfos: " ++ attr ++ " not found")


fromParsing : GameInfos -> List Move -> Game
fromParsing i m =
    let
        boardSize : BoardSized
        boardSize =
            case Debug.log "readFromGameINFOOOOOOOOO" (readFromGameInfos (Debug.log "iii" i) "SZ") of
                Just size ->
                    BoardSized
                        (case String.toInt size of
                            Just s ->
                                s

                            Nothing ->
                                19
                        )

                Nothing ->
                    BoardSized 19

        freshGame : Game
        freshGame =
            fresh boardSize
    in
    { freshGame | infos = i, moves = m, bs = boardSize }


fromSgf : Parser Game
fromSgf =
    Parser.succeed fromParsing
        |. Parser.spaces
        |. Parser.symbol "("
        |. Parser.symbol ";"
        |= gameInfosParser
        |= movesParser
        |. Parser.symbol ")"
        |. Parser.spaces


type alias GameInfo =
    { attribute : String, value : String }


type alias GameInfos =
    List GameInfo


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


gameInfosParser : Parser GameInfos
gameInfosParser =
    Parser.loop [] gameInfosParserHelper


gameInfosParserHelper : GameInfos -> Parser (Parser.Step GameInfos GameInfos)
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



-- fromMoves : List Move -> Game
-- fromMoves moves =
--     { fresh | moves = moves }


toBoard : List Move -> BoardSized -> Board
toBoard moves size =
    List.foldl Board.applyPlay (Board.square (Board.bs2Int size)) moves


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
-- encode : Game -> E.Value
-- encode game =
--     E.object
--         [ ( "moves", E.list Move.encode game.moves )
--         ]
-- decoder : D.Decoder Game
-- decoder =
--     D.map
--         fromMoves
--         (D.field "moves" (D.list Move.decoder))
