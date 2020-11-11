port module Main exposing (main)

import Ant.Icon as AIcon
import Ant.Icons as Icons
import Basics
import Board exposing (Board, movesOf)
import Browser
import Browser.Dom as Bdom exposing (Viewport)
import Browser.Events as BrowserE
import Debug
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Flip exposing (flip)
import Game exposing (Game)
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


main : Program E.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { game : Game
    , table : Maybe Viewport
    , message : Maybe String
    , highlighted : Int
    }


blank : Model
blank =
    { game = Game.fresh
    , table = Nothing
    , message = Nothing
    , highlighted = 0
    }


init : E.Value -> ( Model, Cmd Msg )
init flags =
    ( case D.decodeValue Game.decoder flags of
        Ok game ->
            { blank | game = game, highlighted = List.length game.moves }

        Err _ ->
            blank
    , Cmd.batch
        [ Task.attempt GotTable (Bdom.getViewportOf "table")
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


view : Model -> Html Msg
view model =
    Element.layout
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.centerX
        , Element.centerY
        , Background.color (Element.rgb255 164 143 122)
        ]
        (viewBoardAndPanel model)


viewBoardAndPanel : Model -> Element Msg
viewBoardAndPanel model =
    Element.row
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.centerX
        , Element.centerY
        ]
        [ viewBoard model.table model.highlighted model.game.moves
        , viewPanel model.message model.highlighted model.game.moves
        ]


viewBoard : Maybe Viewport -> Int -> List Move -> Element Msg
viewBoard table highlighted allMoves =
    let
        theSize =
            str (20 * fieldSize)

        svgSize =
            case table of
                Nothing ->
                    0

                Just t ->
                    Basics.min t.viewport.height t.viewport.width

        moves =
            List.take (highlighted + 1) allMoves
    in
    Element.el
        [ Element.width (Element.fillPortion 3)
        , Element.height Element.fill
        , Element.centerX
        , Element.centerY
        , Element.htmlAttribute (HtmlA.id "table")
        ]
        (Element.html
            (Svg.svg
                [ SvgA.version "1.1"
                , SvgA.height (strf svgSize)
                , SvgA.width (strf svgSize)
                , SvgA.viewBox ("0 0 " ++ theSize ++ " " ++ theSize)
                ]
                [ svgRows
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
                , yunziis (Board.movesOf (Game.toBoard moves))
                , clickAreas
                , lastMove (ListExtra.last moves)
                ]
            )
        )


viewPanel : Maybe String -> Int -> List Move -> Element Msg
viewPanel message highlighted moves =
    Element.column
        [ Element.spacing 15
        , Element.centerX
        , Element.alignTop
        , Element.height Element.fill
        , Element.width (Element.fillPortion 1)
        , Element.paddingXY 0 60
        ]
        [ viewButtons
        , viewPlayers (Game.toTurn moves)
        , viewMoves highlighted moves
        , viewMessage message
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


viewMoves : Int -> List Move -> Element Msg
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
        , Element.htmlAttribute (HtmlE.onMouseLeave (Highlight (List.length moves)))
        ]
        (List.indexedMap (viewMove size highlighted) moves)


viewMove : Int -> Int -> Int -> Move -> Element Msg
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

        alpha =
            case index <= highlighted of
                True ->
                    1

                False ->
                    0.3
    in
    Element.row
        [ Element.htmlAttribute (HtmlE.onMouseEnter (Highlight index))
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


viewButtons : Element Msg
viewButtons =
    Element.row
        [ Element.spacing 5
        , Element.paddingEach { top = 0, right = 0, left = 0, bottom = 50 }
        , Element.width Element.fill
        ]
        [ viewButton (viewIcon Icons.undoOutlined) Undo "Undo"
        , viewButton (viewIcon Icons.smallDashOutlined) Pass "Pass"
        , viewButton (viewIcon Icons.downloadOutlined) DownloadGame "Download Game"
        , viewButton (viewIcon Icons.uploadOutlined) SelectGameFile "Upload Game"
        , viewButton (viewIcon Icons.borderlessTableOutlined) ConfirmReset "Reset Game"
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
        (List.append (onPosition pos)
            [ SvgA.r "5"
            , SvgA.fill color
            ]
        )
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
        (List.append (onPosition xy)
            [ SvgA.r "5"
            , SvgA.opacity "0.5"
            , SvgA.fill "transparent"
            , SvgE.onClick (MoveAt xy)
            ]
        )
        []


svgRows : Svg Msg
svgRows =
    Svg.g [] (List.map rowLine (List.range 1 19))


svgCols : Svg Msg
svgCols =
    Svg.g [] (List.map colLine (List.range 1 19))


dot : Int -> Int -> Svg Msg
dot =
    dotWithColor "#dd2d2d2"


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



-- PORTS


port setStorage : String -> Cmd msg


port loadGame : (String -> msg) -> Sub msg


port confirmReset : () -> Cmd msg


port resetGame : (E.Value -> msg) -> Sub msg



-- UPDATE


type Msg
    = MoveAt Position
    | TimedMoveAt Position Time.Posix
    | Pass
    | Undo
    | GotTable (Result Bdom.Error Viewport)
    | Resize
    | Load String
    | Highlight Int
    | DownloadGame
    | SelectGameFile
    | GameFileSelected File
    | GameFileUploaded String
    | ConfirmReset
    | ResetGame E.Value


moveUpdate : Game -> Cmd Msg
moveUpdate game =
    setStorage (Game.toSgf game)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        game =
            model.game

        moves =
            model.game.moves
    in
    case msg of
        MoveAt coordinate ->
            ( model, Task.perform (TimedMoveAt coordinate) Time.now )

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
                    in
                    ( { model | game = newGame, message = Nothing }, moveUpdate newGame )

                Err reason ->
                    ( { model | message = Just (Board.insertionFailureToString reason) }
                    , Cmd.none
                    )

        Pass ->
            -- TODO
            ( model, moveUpdate game )

        Undo ->
            let
                newGame =
                    Game.undoMove game
            in
            ( { model | game = newGame }, moveUpdate newGame )

        GotTable vp ->
            ( { model | table = Result.toMaybe vp }, Cmd.none )

        Resize ->
            ( model, Task.attempt GotTable (Bdom.getViewportOf "table") )

        Load sgfGame ->
            let
                newGame =
                    case Parser.run Game.fromSgf sgfGame of
                        Ok aGame ->
                            aGame

                        Err _ ->
                            game
            in
            ( { model | game = newGame, highlighted = List.length newGame.moves }, Cmd.none )

        Highlight index ->
            ( { model | highlighted = index }, Cmd.none )

        DownloadGame ->
            ( model, Cmd.batch [ Download.string "game.sgf" "text/sgf" (Game.toSgf model.game) ] )

        SelectGameFile ->
            ( model, Cmd.batch [ Select.file [ "text/sgf" ] GameFileSelected ] )

        GameFileSelected sgfFile ->
            ( model, Task.perform GameFileUploaded (File.toString sgfFile) )

        GameFileUploaded sgf ->
            let
                newGame =
                    case Parser.run Game.fromSgf sgf of
                        Ok aGame ->
                            aGame

                        Err _ ->
                            game
            in
            ( { model | game = newGame, highlighted = List.length newGame.moves }, moveUpdate newGame )

        ConfirmReset ->
            ( model, Cmd.batch [ confirmReset () ] )

        ResetGame encodedBoolean ->
            case D.decodeValue D.bool encodedBoolean of
                Ok bool ->
                    case bool of
                        True ->
                            ( { model | game = Game.fresh, highlighted = 0 }, moveUpdate Game.fresh )

                        _ ->
                            ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ BrowserE.onResize (\_ _ -> Resize)
        , loadGame Load
        , resetGame ResetGame
        ]
