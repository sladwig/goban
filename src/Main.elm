port module Main exposing (main)

import Ant.Icon as AIcon
import Ant.Icons as Icons
import Basics
import Board exposing (Board, movesOf)
import Browser
import Browser.Dom as Bdom exposing (Viewport)
import Browser.Events as BrowserE
import Browser.Navigation as Nav
import Debug
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Flip exposing (flip)
import Game exposing (Game, Sgf)
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as D
import Json.Encode as E
import List.Extra as ListExtra
import Move exposing (Move(..))
import Parser exposing ((|.), (|=), Parser)
import Platform.Cmd
import Player exposing (Player)
import Position exposing (Position)
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Svg.Events as SvgE
import Task
import Time
import Url


main : Program E.Value GobanApp Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- MODEL


type ViewGame
    = Last
    | Highlight Int


type alias PlayingGameModel =
    { table : Viewport
    , message : Maybe String
    , highlighted : ViewGame
    , navKey : Nav.Key
    , currentGame : GameId
    , games : Dict String Game
    }


type alias ViewingGameModel =
    { table : Viewport
    , message : Maybe String
    , navKey : Nav.Key
    , games : Dict String Game
    }


type GobanApp
    = Initializing Nav.Key
    | PlayingGame PlayingGameModel
    | ViewingGames ViewingGameModel


type alias IdGame =
    ( GameId, Game )


type alias PossibleIdSgf =
    ( String, String )


type alias IdSgf =
    ( GameId, Sgf )


viewingGames : Nav.Key -> Viewport -> GobanApp
viewingGames key vp =
    ViewingGames
        { table = vp
        , message = Nothing
        , navKey = key
        , games = Dict.empty
        }


playingGame : ViewingGameModel -> IdGame -> GobanApp
playingGame v ( id, game ) =
    let
        updatedGames =
            Dict.insert (gameIdAsString id) game v.games
    in
    PlayingGame
        { table = v.table
        , message = v.message
        , navKey = v.navKey
        , highlighted = Last
        , currentGame = id
        , games = updatedGames
        }


init : E.Value -> Url.Url -> Nav.Key -> ( GobanApp, Cmd Msg )
init flags url key =
    ( Initializing key
    , Cmd.batch
        [ Task.perform UpdateViewport Bdom.getViewport
        ]
    )



-- VIEW


fieldSize : Int
fieldSize =
    10


fieldStart : Int
fieldStart =
    fieldSize


str : Int -> String
str =
    String.fromInt


strf : Float -> String
strf =
    String.fromFloat


tableColor : Element.Color
tableColor =
    Element.rgb255 164 143 122


view : GobanApp -> Browser.Document Msg
view app =
    case app of
        Initializing _ ->
            { title = "Initializing"
            , body =
                [ Element.layout
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.centerX
                    , Element.centerY
                    ]
                    (Element.text "Hola! Initializing")
                ]
            }

        ViewingGames _ ->
            { title = "Viewing"
            , body =
                [ Element.layout
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.centerX
                    , Element.centerY
                    ]
                    (Element.text "Viewing")
                ]
            }

        PlayingGame game ->
            { title = "Goban"
            , body =
                [ Element.layout
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.centerX
                    , Element.centerY
                    ]
                    (viewBoardAndPanel game)
                ]
            }


viewBoardAndPanel : PlayingGameModel -> Element Msg
viewBoardAndPanel model =
    let
        ( id, game ) =
            idGameOf model
    in
    Element.row
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.centerX
        , Element.centerY
        ]
        [ viewBoard model.table model.highlighted game.moves
        , viewPanel model.message model.highlighted ( id, game )
        ]


viewBoard : Viewport -> ViewGame -> List Move -> Element Msg
viewBoard t highlighted allMoves =
    let
        theSize =
            str (20 * fieldSize)

        svgSize =
            -32 + Basics.min t.viewport.height t.viewport.width

        moves =
            case highlighted of
                Last ->
                    allMoves

                Highlight moveNumber ->
                    List.take moveNumber allMoves
    in
    Element.el
        [ Element.width (Element.fillPortion 3)
        , Element.height Element.fill
        , Element.centerX
        , Element.centerY
        , Element.padding 15
        ]
        (Element.el
            [ Element.centerX
            , Element.centerY
            , Border.width 2
            , Border.color <| Element.rgb255 45 45 45
            ]
            (Element.html
                (Svg.svg
                    [ SvgA.version "1.1"
                    , SvgA.height (strf svgSize)
                    , SvgA.width (strf svgSize)
                    , SvgA.viewBox <| "0 0 " ++ theSize ++ " " ++ theSize
                    ]
                    [ rectangle
                    , svgRows
                    , svgCols
                    , dot 4 4
                    , dot 4 10
                    , dot 4 16
                    , dot 10 4
                    , dot 10 10
                    , dot 10 16
                    , dot 16 4
                    , dot 16 10
                    , dot 16 16
                    , yunziis <| Board.movesOf <| Game.toBoard moves
                    , clickAreas
                    , lastMove <| ListExtra.last moves
                    ]
                )
            )
        )


viewPanel : Maybe String -> ViewGame -> IdGame -> Element Msg
viewPanel message highlighted idGame =
    let
        game =
            Tuple.second idGame

        moves =
            game.moves
    in
    Element.column
        [ Element.spacing 15
        , Element.centerX
        , Element.alignTop
        , Element.height Element.fill
        , Element.width (Element.fillPortion 1)
        , Element.paddingXY 0 60
        ]
        [ viewGameButtons
        , viewPlayers (Game.toTurn moves)
        , viewMoves highlighted moves
        , viewMessage message
        , viewButtons idGame
        ]


lastMove : Maybe Move -> Svg Msg
lastMove maybeMove =
    case maybeMove of
        Just move ->
            let
                color =
                    Player.toString (Player.next (Move.playerOf move))

                pos =
                    Move.positionOf move
            in
            Svg.circle
                (onPosition pos
                    ++ [ SvgA.r "1"
                       , SvgA.fill color
                       ]
                )
                []

        Nothing ->
            Svg.circle [] []


viewPlayers : Player -> Element Msg
viewPlayers currentPlayer =
    let
        players =
            case currentPlayer == Player.Black of
                True ->
                    [ viewWhiteTurn, viewBlackTurn ]

                False ->
                    [ viewBlackTurn, viewWhiteTurn ]

        bgColor =
            case currentPlayer of
                Player.White ->
                    whiteColor

                Player.Black ->
                    blackColor
    in
    Element.el [ Element.centerX ]
        (Element.html
            (Svg.svg
                [ SvgA.version "1.1"
                , SvgA.height (strf 32)
                , SvgA.width (strf 64)
                , SvgA.viewBox "0 0 128 64"
                ]
                players
            )
        )


viewWhiteTurn : Svg Msg
viewWhiteTurn =
    Svg.circle
        [ SvgA.r "32"
        , SvgA.cx (str (64 + (32 // 2)))
        , SvgA.cy (str 32)
        , SvgA.fill "white"
        ]
        []


viewBlackTurn : Svg Msg
viewBlackTurn =
    Svg.circle
        [ SvgA.r "32"
        , SvgA.cx (str (64 - (32 // 2)))
        , SvgA.cy (str 32)
        , SvgA.fill "black"
        ]
        []


whiteColor : Float -> Element.Color
whiteColor alpha =
    Element.fromRgb { alpha = alpha, blue = 1, green = 1, red = 1 }


blackColor : Float -> Element.Color
blackColor alpha =
    Element.fromRgb { alpha = alpha, blue = 0, green = 0, red = 0 }


viewMoves : ViewGame -> List Move -> Element Msg
viewMoves highlighted moves =
    let
        size =
            16

        spacing =
            4
    in
    Element.wrappedRow
        [ Element.spacing 3
        , Element.centerX
        , Element.width (Element.px ((size + spacing) * 10))
        , Element.htmlAttribute (HtmlE.onMouseLeave (Highlighting Last))
        ]
        (List.indexedMap (viewMove size highlighted) moves)


viewMove : Int -> ViewGame -> Int -> Move -> Element Msg
viewMove s highlighted index move =
    let
        size =
            Element.px s

        bgColor =
            case Move.playerOf move of
                Player.White ->
                    whiteColor

                Player.Black ->
                    blackColor

        moveNumber =
            index + 1

        alpha =
            case highlighted of
                Last ->
                    12

                Highlight num ->
                    case moveNumber <= num of
                        True ->
                            1

                        False ->
                            0.3
    in
    Element.row
        [ Element.htmlAttribute (HtmlE.onMouseEnter (Highlighting (Highlight moveNumber)))
        ]
        [ Element.el
            [ Background.color (bgColor alpha)
            , Element.width size
            , Element.height size
            , Border.rounded (s // 2)
            ]
            Element.none
        ]


viewMessage : Maybe String -> Element Msg
viewMessage msg =
    case msg of
        Nothing ->
            Element.none

        Just reason ->
            Element.el
                [ Element.alignBottom
                , Element.centerX
                , Element.padding 15
                , Background.color (whiteColor 0.4)
                , Border.rounded 5
                , Font.color tableColor
                ]
                (Element.text reason)


viewGameButtons : Element Msg
viewGameButtons =
    Element.row
        [ Element.spacing 5
        , Element.paddingEach { top = 0, right = 0, left = 0, bottom = 25 }
        , Element.width Element.fill
        ]
        [ viewButton (viewIcon Icons.undoOutlined) Undo "Undo"
        , viewButton (viewIcon Icons.smallDashOutlined) Pass "Pass"
        ]


viewButtons : IdGame -> Element Msg
viewButtons ( id, game ) =
    Element.row
        [ Element.alignBottom
        , Element.spacing 5
        , Element.paddingEach { top = 25, right = 0, left = 0, bottom = 0 }
        , Element.width Element.fill
        ]
        [ viewButton (viewIcon Icons.downloadOutlined) (DownloadGame (Game.toSgf game)) "Download Game"
        , viewButton (viewIcon Icons.uploadOutlined) SelectGameFile "Upload Game"
        , viewButton (viewIcon Icons.borderlessTableOutlined) (ConfirmReset id) "Reset Game"
        ]


viewButton : Element Msg -> Msg -> String -> Element Msg
viewButton label msg title =
    Input.button
        [ Background.color (Element.rgba255 238 238 238 0.8)
        , Element.mouseOver [ Background.color (Element.rgba255 238 238 238 1) ]
        , Element.padding 5
        , Border.rounded 24
        , Element.centerX
        , Font.color (Element.rgb255 164 143 122)
        , Element.htmlAttribute (HtmlA.title title)
        ]
        { onPress = Just msg
        , label = label
        }


viewIcon : (List (AIcon.Attribute msg) -> Element Msg) -> Element Msg
viewIcon icon =
    icon
        [ AIcon.width 24
        , AIcon.height 24
        ]


yunziis : List Move -> Svg Msg
yunziis moves =
    Svg.g [] (List.map yunzi moves)


onPosition : Position -> List (Svg.Attribute Msg)
onPosition pos =
    [ SvgA.cx (str (fieldSize * Position.x pos))
    , SvgA.cy (str (fieldSize * Position.y pos))
    ]


yunzi : Move -> Svg Msg
yunzi move =
    let
        color =
            Player.toString (Move.playerOf move)

        pos =
            Move.positionOf move
    in
    Svg.circle
        (onPosition pos ++ [ SvgA.r "5", SvgA.fill color ])
        []


clickAreas : Svg Msg
clickAreas =
    Svg.g [] (List.map hoplas (List.range 1 19))


hoplas : Int -> Svg Msg
hoplas row =
    Svg.g [] (List.map clicky (List.map (Tuple.pair row) (List.range 1 19)))


clicky : Position -> Svg Msg
clicky xy =
    Svg.circle
        ([ SvgA.r "5"
         , SvgA.opacity "0.5"
         , SvgA.fill "transparent"
         , SvgE.onClick (MoveAt xy)
         ]
            ++ onPosition xy
        )
        []


svgRows : Svg Msg
svgRows =
    Svg.g [] (List.map rowLine (List.range 2 18))


svgCols : Svg Msg
svgCols =
    Svg.g [] (List.map colLine (List.range 2 18))


dot : Int -> Int -> Svg Msg
dot =
    dotWithColor "#2d2d2d"


dotWithColor : String -> Int -> Int -> Svg Msg
dotWithColor color x y =
    Svg.circle
        [ SvgA.r "1.5"
        , SvgA.cx (str (fieldSize * x))
        , SvgA.cy (str (fieldSize * y))
        , SvgA.fill color
        ]
        []


rowAt : Int -> List (Svg.Attribute Msg)
rowAt x =
    [ SvgA.y1 (str (fieldSize * x))
    , SvgA.y2 (str (fieldSize * x))
    , SvgA.x1 (str fieldStart)
    , SvgA.x2 (str (fieldSize * 19))
    ]


rowLine : Int -> Svg Msg
rowLine idx =
    Svg.line (List.append (rowAt idx) [ SvgA.strokeWidth "0.5", SvgA.stroke "#2d2d2d" ]) []


colAt : Int -> List (Svg.Attribute Msg)
colAt x =
    [ SvgA.x1 (str (fieldSize * x))
    , SvgA.x2 (str (fieldSize * x))
    , SvgA.y1 (str fieldStart)
    , SvgA.y2 (str (fieldSize * 19))
    ]


colLine : Int -> Svg Msg
colLine idx =
    Svg.line (List.append (colAt idx) [ SvgA.strokeWidth "0.5", SvgA.stroke "#2d2d2d" ]) []


rectangle : Svg Msg
rectangle =
    let
        start =
            str fieldStart

        end =
            str (fieldSize * 18)
    in
    Svg.rect
        [ SvgA.x start
        , SvgA.y start
        , SvgA.width end
        , SvgA.height end
        , SvgA.strokeWidth "0.5"
        , SvgA.stroke "#2d2d2d"
        , SvgA.fill "none"
        ]
        []


idGameOf : { a | currentGame : GameId, games : Dict String Game } -> IdGame
idGameOf goban =
    ( goban.currentGame, Maybe.withDefault Game.fresh <| Dict.get (gameIdAsString goban.currentGame) goban.games )



-- PORTS


port enterRoom : String -> Cmd msg


port updateGame : ( String, String ) -> Cmd msg


port loadGame : (( String, String ) -> msg) -> Sub msg


port confirmReset : String -> Cmd msg


port resetGame : (E.Value -> msg) -> Sub msg



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | UpdateViewport Viewport
    | Resize
    | MoveAt Position
    | TimedMoveAt Position Time.Posix
    | Pass
    | Undo
    | Load ( String, String )
    | Highlighting ViewGame
    | DownloadGame Sgf
    | SelectGameFile
    | GameFileSelected File
    | GameFileUploaded String
    | ConfirmReset GameId
    | ResetGame E.Value


type GameId
    = GameId String


gameIdAsString : GameId -> String
gameIdAsString (GameId id) =
    id


notify : ( GameId, Game ) -> Cmd Msg
notify ( id, game ) =
    updateGame <| ( gameIdAsString <| id, Game.toString <| Game.toSgf game )


updatePlayingGame : { a | currentGame : GameId, games : Dict String Game } -> Game -> { a | currentGame : GameId, games : Dict String Game }
updatePlayingGame p newGame =
    let
        newGames =
            Dict.insert (gameIdAsString p.currentGame) newGame p.games
    in
    { p | games = newGames }


update : Msg -> GobanApp -> ( GobanApp, Cmd Msg )
update msg app =
    case app of
        Initializing key ->
            case msg of
                UpdateViewport vp ->
                    ( viewingGames key vp, Cmd.batch [ enterRoom "ab0dc465-fd83-4ad9-86d1-04da44a90f4c" ] )

                _ ->
                    ( app, Cmd.none )

        ViewingGames goban ->
            case msg of
                Load ( id, sgf ) ->
                    let
                        newGame =
                            case Parser.run Game.fromSgf sgf of
                                Ok aGame ->
                                    aGame

                                Err _ ->
                                    Game.fresh

                        newAppState =
                            playingGame goban ( GameId id, newGame )
                    in
                    ( newAppState, Cmd.none )

                _ ->
                    ( app, Cmd.none )

        PlayingGame goban ->
            let
                idGame =
                    idGameOf goban

                game =
                    Tuple.second idGame

                moves =
                    game.moves
            in
            case msg of
                MoveAt coordinate ->
                    ( PlayingGame goban, Task.perform (TimedMoveAt coordinate) Time.now )

                TimedMoveAt coordinate time ->
                    let
                        move =
                            Move.fromPlayerAndPositionAndTime (Game.toTurn moves) coordinate time
                    in
                    case Board.play move (Game.toBoard moves) of
                        Ok board ->
                            let
                                newGame =
                                    Game.makeMove game move

                                newAppState =
                                    updatePlayingGame goban newGame
                            in
                            ( PlayingGame { newAppState | message = Nothing }, notify ( goban.currentGame, newGame ) )

                        Err reason ->
                            ( PlayingGame { goban | message = Just (Board.insertionFailureToString reason) }
                            , Cmd.none
                            )

                Pass ->
                    -- TODO
                    ( PlayingGame goban, notify ( goban.currentGame, game ) )

                Undo ->
                    let
                        newGame =
                            Game.undoMove game

                        newGoban =
                            updatePlayingGame goban newGame
                    in
                    ( PlayingGame newGoban, notify ( goban.currentGame, newGame ) )

                Resize ->
                    ( PlayingGame goban, Task.perform UpdateViewport Bdom.getViewport )

                Load ( id, sgf ) ->
                    let
                        newGame =
                            case Parser.run Game.fromSgf sgf of
                                Ok aGame ->
                                    aGame

                                Err _ ->
                                    game

                        newAppState =
                            updatePlayingGame goban newGame
                    in
                    ( PlayingGame newAppState, Cmd.none )

                Highlighting index ->
                    ( PlayingGame { goban | highlighted = index }, Cmd.none )

                DownloadGame sgf ->
                    ( PlayingGame goban, Cmd.batch [ Download.string "game.sgf" "text/sgf" (Game.toString sgf) ] )

                SelectGameFile ->
                    ( PlayingGame goban, Cmd.batch [ Select.file [ "text/sgf" ] GameFileSelected ] )

                GameFileSelected sgfFile ->
                    ( PlayingGame goban, Task.perform GameFileUploaded (File.toString sgfFile) )

                GameFileUploaded sgf ->
                    let
                        newGame =
                            case Parser.run Game.fromSgf sgf of
                                Ok aGame ->
                                    aGame

                                Err _ ->
                                    game

                        newAppState =
                            updatePlayingGame goban newGame
                    in
                    -- TODO add new id generation
                    ( PlayingGame newAppState, notify ( goban.currentGame, newGame ) )

                ConfirmReset (GameId id) ->
                    ( PlayingGame goban, Cmd.batch [ confirmReset id ] )

                ResetGame encodedBoolean ->
                    case D.decodeValue D.bool encodedBoolean of
                        Ok bool ->
                            case bool of
                                True ->
                                    let
                                        newAppState =
                                            updatePlayingGame goban Game.fresh
                                    in
                                    ( PlayingGame newAppState, notify ( goban.currentGame, Game.fresh ) )

                                _ ->
                                    ( PlayingGame goban, Cmd.none )

                        Err _ ->
                            ( PlayingGame goban, Cmd.none )

                LinkClicked urlRequest ->
                    case urlRequest of
                        Browser.Internal url ->
                            ( PlayingGame goban, Nav.pushUrl goban.navKey (Url.toString url) )

                        Browser.External href ->
                            ( PlayingGame goban, Nav.load href )

                UrlChanged _ ->
                    ( PlayingGame goban, Cmd.none )

                UpdateViewport _ ->
                    ( PlayingGame goban, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : GobanApp -> Sub Msg
subscriptions _ =
    Sub.batch
        [ BrowserE.onResize (\_ _ -> Resize)
        , loadGame Load
        , resetGame ResetGame
        ]
