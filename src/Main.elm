port module Main exposing (main)

-- import Math
-- import Element.Color as Color

import Ant.Icon as AIcon
import Ant.Icons as Icons
import Basics
import Board exposing (BoardSized(..), bs2Int, movesOf)
import Browser
import Browser.Dom as Bdom exposing (Viewport)
import Browser.Events as BrowserE
import Browser.Navigation as Nav
import Debug
import Dict exposing (Dict)
import Element as El exposing (Element, alignBottom, alignLeft, alignRight, alignTop, centerX, centerY, clip, clipX, clipY, column, el, height, htmlAttribute, moveDown, moveUp, none, padding, paddingEach, paddingXY, px, rgb255, rgba255, rotate, row, scrollbarX, scrollbarY, scrollbars, spaceEvenly, spacing, spacingXY, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Flip exposing (flip)
import Game exposing (Game, Sgf)
import Html exposing (Html)
import Html.Attributes as HtmlA exposing (style)
import Html.Events as HtmlE
import Json.Decode as D
import Json.Encode as E
import List.Extra as ListExtra
import Move exposing (Move(..))
import Parser exposing ((|.), (|=), Parser)
import Platform.Cmd
import Player exposing (Player(..))
import Position exposing (Position)
import Process
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


type ZoomState
    = ZoomedOut
    | ZoomedIn
    | TriggeredZoomIn


type MenuState
    = Hidden
    | Visible
    | PreGame


type alias PlayingGameModel =
    { table : Viewport
    , message : Maybe String
    , highlighted : ViewGame
    , navKey : Nav.Key
    , currentGame : GameId
    , games : Dict String Game

    -- , boardSize : BoardSized
    , showMenu : MenuState
    , tableTurn : Int
    , zoomState : ZoomState
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
        gameHasStarted =
            not (List.isEmpty game.moves)

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

        -- , boardSize = BoardSized 19
        , tableTurn = 0
        , showMenu =
            if gameHasStarted then
                Visible

            else
                PreGame
        , zoomState = ZoomedOut

        -- , boardSize =
        }


init : E.Value -> Url.Url -> Nav.Key -> ( GobanApp, Cmd Msg )
init flags url key =
    ( Initializing key
    , Cmd.batch
        [ Task.perform UpdateViewport Bdom.getViewport
        ]
    )



-- VIEW


str : Int -> String
str =
    String.fromInt


strf : Float -> String
strf =
    String.fromFloat


tableColor : El.Color
tableColor =
    El.rgb255 164 143 122



-- the handicap positions


handiCapDots : Int -> BoardSized -> List (Svg Msg)
handiCapDots fieldSize boardSize =
    let
        dotSized =
            dot fieldSize
    in
    case boardSize of
        BoardSized 19 ->
            [ dotSized 4 4
            , dotSized 4 10
            , dotSized 4 16
            , dotSized 10 4
            , dotSized 10 10
            , dotSized 10 16
            , dotSized 16 4
            , dotSized 16 10
            , dotSized 16 16
            ]

        BoardSized 13 ->
            [ dotSized 4 4
            , dotSized 4 10
            , dotSized 10 4
            , dotSized 10 10
            , dotSized 7 7
            ]

        BoardSized 9 ->
            [ dotSized 3 3
            , dotSized 7 3
            , dotSized 3 7
            , dotSized 7 7
            ]

        _ ->
            []


view : GobanApp -> Browser.Document Msg
view app =
    case app of
        Initializing _ ->
            { title = "Initializing"
            , body =
                [ El.layout
                    [ El.width El.fill
                    , El.height El.fill
                    , El.centerX
                    , El.centerY
                    ]
                    (El.text "Hola! Initializing")
                ]
            }

        ViewingGames _ ->
            { title = "Viewing"
            , body =
                [ El.layout
                    [ El.width El.fill
                    , El.height El.fill
                    , El.centerX
                    , El.centerY
                    ]
                    (El.text "Viewing")
                ]
            }

        PlayingGame game ->
            let
                ( id, gameGame ) =
                    idGameOf game
            in
            { title = "Goban"
            , body =
                [ El.layout
                    [ El.width El.fill
                    , El.height El.fill
                    , El.centerX
                    , El.centerY
                    , El.inFront (viewLeftPanel game.tableTurn game.boardSize game.message game.highlighted ( id, gameGame ) game.showMenu)
                    , El.inFront (viewPanel game.tableTurn game.boardSize game.message game.highlighted ( id, gameGame ) game.showMenu)
                    ]
                    (viewBoardAndPanel game)
                ]
            }


viewBoardAndPanel : PlayingGameModel -> Element Msg
viewBoardAndPanel m =
    let
        ( id, game ) =
            idGameOf m
    in
    El.row
        [ El.width El.fill
        , El.height El.fill
        , El.centerX
        , El.centerY
        , El.htmlAttribute (HtmlA.style "overflow" "hidden")
        , htmlAttribute (HtmlE.onDoubleClick ZoomInOut)
        ]
        [ viewBoard m.zoomState m.tableTurn m.table m.highlighted game.moves m.boardSize

        -- , viewPanel m.tableTurn m.boardSize m.message m.highlighted ( id, game ) m.showMenu
        ]


viewBoard : ZoomState -> Int -> Viewport -> ViewGame -> List Move -> BoardSized -> Element Msg
viewBoard zoomState turns t highlighted allMoves boardSize =
    let
        svgSize =
            -32 + Basics.min t.viewport.height t.viewport.width

        bs =
            bs2Int boardSize

        fieldSize =
            round (svgSize / toFloat bs + 2)

        viewBoxSize =
            str ((bs + 1) * fieldSize)

        moves =
            case highlighted of
                Last ->
                    allMoves

                Highlight moveNumber ->
                    List.take moveNumber allMoves

        rotation =
            turns * 90

        zoom =
            if zoomState == ZoomedIn then
                1.8

            else
                1
    in
    El.el
        [ El.width El.fill

        -- El.width (El.fillPortion 3)
        , El.height El.fill
        , El.centerX
        , El.centerY
        , El.scale zoom
        , El.padding 15
        , El.rotate (degrees (toFloat rotation))
        , El.htmlAttribute (HtmlA.style "transition" "transform 150ms ease-in-out")
        , htmlAttribute (HtmlE.onDoubleClick ZoomInOut)

        -- , Element
        ]
        (El.el
            [ El.centerX
            , El.centerY
            , Border.width 2
            , Border.color <| El.rgb255 45 45 45
            ]
            (El.html
                (Svg.svg
                    [ SvgA.version "1.1"

                    -- , SvgA.height El.fill
                    --
                    , SvgA.width (strf svgSize)
                    , SvgA.height (strf svgSize)
                    , SvgA.viewBox <| "0 0 " ++ viewBoxSize ++ " " ++ viewBoxSize

                    -- , SvgA.style "widkth: 100%;hkeight: 100%;background: greekn;"
                    -- [ El.height El.fill
                    -- , El.width El.fill
                    -- ]
                    ]
                    (List.append
                        ([ rectangle fieldSize boardSize
                         , svgRows fieldSize boardSize
                         , svgCols fieldSize boardSize
                         ]
                            ++ handiCapDots fieldSize boardSize
                        )
                        [ yunziis fieldSize <| Board.movesOf <| Game.toBoard moves boardSize
                        , clickAreas fieldSize boardSize
                        , lastMove fieldSize <| ListExtra.last moves
                        ]
                    )
                )
            )
        )


lastMove : Int -> Maybe Move -> Svg Msg
lastMove fieldSize maybeMove =
    case maybeMove of
        Just move ->
            let
                color =
                    Player.toString (Player.next (Move.playerOf move))

                pos =
                    Move.positionOf move
            in
            Svg.circle
                (onPosition fieldSize pos
                    ++ [ SvgA.r (String.fromFloat (toFloat fieldSize / 20))
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
    El.el [ El.centerX ]
        (El.html
            (Svg.svg
                [ SvgA.version "1.1"
                , SvgA.height (strf 48)
                , SvgA.width (strf 96)
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


whiteColor : Float -> El.Color
whiteColor alpha =
    El.fromRgb { alpha = alpha, blue = 1, green = 1, red = 1 }


blackColor : Float -> El.Color
blackColor alpha =
    El.fromRgb { alpha = alpha, blue = 0, green = 0, red = 0 }


viewMoves : ViewGame -> List Move -> Element Msg
viewMoves highlighted moves =
    let
        size =
            12

        spacing =
            2
    in
    El.wrappedRow
        [ El.spacing spacing
        , El.centerX
        , scrollbarY

        -- , El.width (El.px ((size + spacing) * 1))
        , padding 15
        , El.htmlAttribute (HtmlE.onMouseLeave (Highlighting Last))
        ]
        (List.indexedMap (viewMove size highlighted) moves)


viewMove : Int -> ViewGame -> Int -> Move -> Element Msg
viewMove s highlighted index move =
    let
        size =
            El.px s

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

        -- handleTouch : Touch.Event -> Msg
        -- handleTouch e =
        --     Highlighting (Highlight moveNumber)
        -- endTouch : Touch.Event -> Msg
        -- endTouch e =
        --     Highlighting Last
    in
    el
        [ El.htmlAttribute (HtmlE.onMouseEnter (Highlighting (Highlight moveNumber)))

        -- , padding 15
        -- , El.htmlAttribute (HtmlE.onMouseLeave (Highlighting Last))
        -- , El.htmlAttribute
        --     (Touch.onMove handleTouch)
        -- , El.htmlAttribute
        --     (Touch.onStart handleTouch)
        -- , El.htmlAttribute
        --     (Touch.onEnd handleTouch)
        , Background.color
            (bgColor alpha)
        , padding 15
        , El.width size
        , El.height size
        , Border.rounded (s // 2)
        ]
        none


viewMessage : Maybe String -> Element Msg
viewMessage msg =
    let
        attrs =
            [ El.alignBottom
            , El.centerX
            , El.centerY
            , El.padding 15
            , width (px 50)
            , Background.color (whiteColor 0.4)
            , Border.rounded 5
            , Font.color tableColor
            , htmlAttribute (style "writing-mode" "vertical-rl")
            , htmlAttribute (style "text-orientation" "mixed")
            , htmlAttribute (style "transition" "height 15000ms ease-in")
            ]
    in
    case msg of
        Nothing ->
            el
                (attrs ++ [ El.transparent True ])
                none

        Just reason ->
            el
                attrs
                (El.text reason)


iconSize =
    28


halfIconSize =
    iconSize // 2


viewCurrentPlayer : Int -> Player -> Element Msg
viewCurrentPlayer size currentPlayer =
    let
        playerYunzi key opt =
            Keyed.el
                ([ width (px size)
                 , height (px size)
                 , Border.rounded size
                 ]
                    ++ opt
                )
                ( key, none )

        -- white : List El.Attribute -> Element Msg
        whitePlayer =
            playerYunzi "white"
                ([ Background.color (rgb255 255 255 255)
                 , moveDown 20
                 ]
                 -- ++ opt
                )

        blackPlayer =
            playerYunzi "black"
                ([ Background.color (rgb255 0 0 0)

                 --  , moveUp 10
                 ]
                 -- ++ opt
                )

        ( playing, waiting ) =
            case currentPlayer of
                White ->
                    ( whitePlayer, blackPlayer )

                Black ->
                    ( blackPlayer, whitePlayer )
    in
    el
        [ width (px size)
        , height (px (size + (size // 2)))
        , El.behindContent waiting
        , El.inFront playing

        -- , Background.color (rgb255 0 244 0)
        ]
        none


viewLeftPanel : Int -> BoardSized -> Maybe String -> ViewGame -> IdGame -> MenuState -> Element Msg
viewLeftPanel turns boardSize message highlighted idGame showMenu =
    let
        undoBtn =
            viewButton (viewIcon Icons.undoOutlined) Undo "Undo"

        game =
            Tuple.second idGame

        moves =
            game.moves

        gameHasNotStarted =
            List.isEmpty game.moves
    in
    row [ height El.fill ]
        [ column
            [ height El.fill
            , width (El.px 80)
            , centerY
            , centerX

            -- , Background.color (rgba255 44 155 155 33)
            -- , spaceEvenly
            -- , width (El.fillPortion 1)
            -- , El.alpha 0.3
            ]
            [ --   undoBtn
              -- , undoBtn
              -- , el
              --     [ Background.color (El.rgb255 33 233 33)
              --     , width (px 15)
              --     , height El.fill
              --     ]
              -- none
              -- , row
              --     [ El.spacing 15
              --     , El.centerX
              --     , El.alignTop
              --     , El.centerX
              --     -- , Background.Color (El.rgb255 33 33 33)
              --     , Background.color (El.rgb255 33 33 33)
              --     , El.height El.fill
              --     , El.width (El.fillPortion 1)
              --     , El.paddingXY 15 15
              --     ]
              -- [
              -- , viewMenu turns boardSize idGame gameHasNotStarted showMenu
              -- [ viewGameButtons
              -- ,
              viewCurrentPlayer (round (iconSize * 1.5)) (Game.toTurn moves)
            , undoBtn -- , viewButtons turns boardSize idGame gameHasNotStarted

            -- ]
            ]

        -- , viewMoves
        --     highlighted
        --     moves
        ]


viewPanel : Int -> BoardSized -> Maybe String -> ViewGame -> IdGame -> MenuState -> Element Msg
viewPanel turns boardSize message highlighted idGame showMenu =
    let
        game =
            Tuple.second idGame

        moves =
            game.moves

        gameHasNotStarted =
            List.isEmpty game.moves
    in
    row
        [ height El.fill
        , alignRight

        -- , width (El.fillPortion 1)
        , El.alpha 0.3
        ]
        [ viewMessage message

        -- , viewMoves highlighted moves
        -- , row
        --     [ El.spacing 15
        --     , El.centerX
        --     , El.alignTop
        --     , El.centerX
        --     -- , Background.Color (El.rgb255 33 33 33)
        --     , Background.color (El.rgb255 33 33 33)
        --     , El.height El.fill
        --     , El.width (El.fillPortion 1)
        --     , El.paddingXY 15 15
        --     ]
        -- [
        , viewMenu turns boardSize idGame gameHasNotStarted showMenu

        -- [ viewGameButtons
        -- , viewCurrentPlayer (round (iconSize * 1.5)) (Game.toTurn moves)
        -- , viewButtons turns boardSize idGame gameHasNotStarted
        -- ]
        ]


viewGameButtons : Element Msg
viewGameButtons =
    El.row
        [ El.spacing halfIconSize
        , El.paddingEach { top = 0, right = 0, left = 0, bottom = 25 }
        , El.width El.fill
        ]
        [ viewButton (viewIcon Icons.undoOutlined) Undo "Undo"
        , viewButton (viewIcon Icons.smallDashOutlined) Pass "Pass"
        ]


viewMenu : Int -> BoardSized -> IdGame -> Bool -> MenuState -> Element Msg
viewMenu turns boardSize ( id, game ) gameNotStarted showMenu =
    let
        undoBtn =
            viewButton (viewIcon Icons.undoOutlined) Undo "Undo"

        menuBtn =
            viewButton (viewIcon Icons.smallDashOutlined) ShowMenu "Menu"

        downloadGameBtn =
            viewButton (viewIcon Icons.downloadOutlined) (DownloadGame (Game.toSgf game)) "Download Game"

        uploadGameBtn =
            viewButton (viewIcon Icons.uploadOutlined)
                SelectGameFile
                "Upload Game"

        resetGameBtn =
            viewButton
                (viewIcon Icons.borderlessTableOutlined)
                (ConfirmReset id)
                "Reset Game"

        turnTableBtn =
            aButton
                [ El.rotate (degrees (toFloat rotation))
                , El.htmlAttribute (HtmlA.style "transition" "transform 90ms ease-in-out")
                ]
                (viewIcon Icons.caretUpFilled)
                TurnBoard
                "Turn Table"

        changeBoardSizeBtn =
            viewButton
                (El.row
                    [ El.width (El.px iconSize)
                    , El.height
                        (El.px iconSize)
                    , El.htmlAttribute
                        (HtmlA.style "display" "flex")
                    , El.htmlAttribute
                        (HtmlA.style "alignItems" "center")
                    , El.htmlAttribute
                        (HtmlA.style "justifyContent" "center")
                    , El.htmlAttribute
                        (HtmlA.style "textAlign" "center")
                    ]
                    [ El.text (String.fromInt (bs2Int boardSize)) ]
                )
                ChangeBoardSize
                "Change Board Size"

        gameButtons =
            if gameNotStarted then
                [ changeBoardSizeBtn
                ]

            else
                []

        rotation =
            turns * 90

        rowAttrs =
            [ El.centerX
            , El.centerY

            -- , Background.color (rgb255 244 0 0)
            , El.spacing halfIconSize
            , El.paddingEach { top = 0, right = 15, left = 15, bottom = 0 }
            , El.width El.fill
            ]
    in
    column
        rowAttrs
        (case showMenu of
            Hidden ->
                [ menuBtn ]

            Visible ->
                [ undoBtn
                , menuBtn
                , downloadGameBtn
                , uploadGameBtn
                , resetGameBtn
                , turnTableBtn
                ]

            PreGame ->
                [ changeBoardSizeBtn
                , menuBtn
                , downloadGameBtn
                , uploadGameBtn
                , resetGameBtn
                , turnTableBtn
                ]
        )


viewButtons : Int -> BoardSized -> IdGame -> Bool -> Element Msg
viewButtons turns boardSize ( id, game ) gameNotStarted =
    let
        downloadGameBtn =
            viewButton (viewIcon Icons.downloadOutlined) (DownloadGame (Game.toSgf game)) "Download Game"

        uploadGameBtn =
            viewButton (viewIcon Icons.uploadOutlined)
                SelectGameFile
                "Upload Game"

        resetGameBtn =
            viewButton
                (viewIcon Icons.borderlessTableOutlined)
                (ConfirmReset id)
                "Reset Game"

        turnTableBtn =
            aButton
                [ El.rotate (degrees (toFloat rotation))
                , El.htmlAttribute (HtmlA.style "transition" "transform 90ms ease-in-out")
                ]
                (viewIcon Icons.caretUpFilled)
                TurnBoard
                "Turn Table"

        changeBoardSizeBtn =
            viewButton
                (El.row
                    [ El.width (El.px iconSize)
                    , El.height
                        (El.px iconSize)
                    , El.htmlAttribute
                        (HtmlA.style "display" "flex")
                    , El.htmlAttribute
                        (HtmlA.style "alignItems" "center")
                    , El.htmlAttribute
                        (HtmlA.style "justifyContent" "center")
                    , El.htmlAttribute
                        (HtmlA.style "textAlign" "center")
                    ]
                    [ El.text (String.fromInt (bs2Int boardSize)) ]
                )
                ChangeBoardSize
                "Change Board Size"

        gameButtons =
            if gameNotStarted then
                [ changeBoardSizeBtn
                ]

            else
                []

        -- El.column
        --     rowAttrs
        --     [ El.wrappedRow
        --         rowAttrs
        --         [ downloadGameBtn
        --         , uploadGameBtn
        --         ]
        --     , El.wrappedRow
        --         rowAttrs
        --         [ resetGameBtn
        --         , turnTableBtn
        --         ]
        --     ]
        rotation =
            turns * 90

        rowAttrs =
            [ El.centerX
            , El.alignBottom
            , El.spacing halfIconSize
            , El.paddingEach { top = 5, right = 15, left = 15, bottom = 0 }
            , El.width El.fill

            -- , El.
            ]
    in
    El.wrappedRow
        rowAttrs
        [ downloadGameBtn
        , uploadGameBtn
        , resetGameBtn
        , turnTableBtn
        ]


viewButton : Element Msg -> Msg -> String -> Element Msg
viewButton label msg title =
    aButton [] label msg title


aButton : List (El.Attribute Msg) -> Element Msg -> Msg -> String -> Element Msg
aButton attrs label msg title =
    Input.button
        ([ Background.color (El.rgba255 238 238 238 0.8)
         , El.mouseOver [ Background.color (El.rgba255 238 238 238 1) ]
         , El.padding halfIconSize
         , Border.rounded iconSize
         , El.centerX
         , Font.color (El.rgb255 164 143 122)
         , El.htmlAttribute (HtmlA.title title)
         ]
            ++ attrs
        )
        { onPress = Just msg
        , label = label
        }


viewIcon : (List (AIcon.Attribute msg) -> Element Msg) -> Element Msg
viewIcon icon =
    icon
        [ AIcon.width iconSize
        , AIcon.height iconSize
        ]


yunziis : Int -> List Move -> Svg Msg
yunziis fieldSize moves =
    Svg.g [] (List.map (yunzi fieldSize) moves)


onPosition : Int -> Position -> List (Svg.Attribute Msg)
onPosition fieldSize pos =
    [ SvgA.cx (str (fieldSize * Position.x pos))
    , SvgA.cy (str (fieldSize * Position.y pos))
    ]


yunzi : Int -> Move -> Svg Msg
yunzi fieldSize move =
    let
        color =
            Player.toString (Move.playerOf move)

        pos =
            Move.positionOf move
    in
    Svg.circle
        (onPosition fieldSize pos ++ [ SvgA.r (String.fromFloat (toFloat fieldSize / 2.0)), SvgA.fill color ])
        []


clickAreas : Int -> BoardSized -> Svg Msg
clickAreas fieldSize boardSize =
    let
        hoplas : Int -> BoardSized -> Int -> Svg Msg
        hoplas fs bs row =
            Svg.g [] (List.map (clicky fieldSize) (List.map (Tuple.pair row) (List.range 1 (bs2Int boardSize))))
    in
    Svg.g [] (List.map (hoplas fieldSize boardSize) (List.range 1 (bs2Int boardSize)))


clicky : Int -> Position -> Svg Msg
clicky fieldSize xy =
    Svg.circle
        ([ SvgA.r (String.fromFloat (toFloat fieldSize / 2.0))
         , SvgA.opacity "0.5"
         , SvgA.fill "transparent"

         --  , SvgE.onMouseDown TriggerDelayedZoom
         --  , SvgE.onMouseUp ZoomOutOfBoard
         , SvgE.onClick (MoveAt xy)
         ]
            ++ onPosition fieldSize xy
        )
        []


svgRows : Int -> BoardSized -> Svg Msg
svgRows fieldSize boardSize =
    let
        bs =
            bs2Int boardSize - 1
    in
    Svg.g [] (List.map (rowLine fieldSize boardSize) (List.range 2 bs))


svgCols : Int -> BoardSized -> Svg Msg
svgCols fieldSize boardSize =
    let
        bs =
            bs2Int boardSize - 1
    in
    Svg.g [] (List.map (colLine fieldSize boardSize) (List.range 2 bs))


dot : Int -> Int -> Int -> Svg Msg
dot fieldSize =
    dotWithColor fieldSize "#2d2d2d"


dotWithColor : Int -> String -> Int -> Int -> Svg Msg
dotWithColor fieldSize color x y =
    Svg.circle
        [ SvgA.r (String.fromFloat (toFloat fieldSize / 10.0))
        , SvgA.cx (str (fieldSize * x))
        , SvgA.cy (str (fieldSize * y))
        , SvgA.fill color
        ]
        []


rowAt : Int -> BoardSized -> Int -> List (Svg.Attribute Msg)
rowAt fieldSize boardSize x =
    [ SvgA.x1 (str fieldSize)
    , SvgA.y1 (str (fieldSize * x))
    , SvgA.x2 (str (fieldSize * bs2Int boardSize))
    , SvgA.y2 (str (fieldSize * x))
    ]


rowLine : Int -> BoardSized -> Int -> Svg Msg
rowLine fieldSize boardSize idx =
    Svg.line (List.append (rowAt fieldSize boardSize idx) [ SvgA.strokeWidth "0.5", SvgA.stroke "#2d2d2d" ]) []


colAt : Int -> BoardSized -> Int -> List (Svg.Attribute Msg)
colAt fieldSize boardSize x =
    [ SvgA.x1 (str (fieldSize * x))
    , SvgA.x2 (str (fieldSize * x))
    , SvgA.y1 (str fieldSize)
    , SvgA.y2 (str (fieldSize * bs2Int boardSize))
    ]


colLine : Int -> BoardSized -> Int -> Svg Msg
colLine fieldSize boardSize idx =
    Svg.line (List.append (colAt fieldSize boardSize idx) [ SvgA.strokeWidth "0.5", SvgA.stroke "#2d2d2d" ]) []


rectangle : Int -> BoardSized -> Svg Msg
rectangle fieldSize boardSize =
    let
        fieldStart =
            fieldSize

        start =
            str fieldStart

        end =
            str (fieldSize * (bs2Int boardSize - 1))
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
    ( goban.currentGame, Maybe.withDefault (Game.fresh (BoardSized 19)) <| Dict.get (gameIdAsString goban.currentGame) goban.games )



-- PORTS


port enterRoom : String -> Cmd msg


port updateGame : ( String, String ) -> Cmd msg


port loadGame : (( String, String ) -> msg) -> Sub msg


port confirmReset : String -> Cmd msg


port resetGame : (( String, Bool ) -> msg) -> Sub msg



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | UpdateViewport Viewport
    | Resize
    | MoveAt Position
      -- | Handicap
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
    | ResetGame ( String, Bool )
    | TurnBoard
    | ChangeBoardSize
    | ShowMenu
    | TriggerDelayedZoom
    | ZoomInOnBoard
    | ZoomOutOfBoard
    | ZoomInOut


type GameId
    = GameId String


nextMenuState : Bool -> MenuState -> MenuState
nextMenuState gameStarted thisState =
    case thisState of
        Hidden ->
            if gameStarted then
                Visible

            else
                Hidden

        Visible ->
            Hidden

        PreGame ->
            Hidden


setTimer : Msg -> Float -> Cmd Msg
setTimer msg time =
    Process.sleep time
        |> Task.attempt (\_ -> msg)


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
                                    Game.fresh (BoardSized 19)

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

                boardSize =
                    goban.boardSize

                l =
                    Debug.log "boardSize" boardSize
            in
            case msg of
                MoveAt coordinate ->
                    ( PlayingGame goban, Task.perform (TimedMoveAt coordinate) Time.now )

                TimedMoveAt coordinate time ->
                    let
                        move =
                            Move.fromPlayerAndPositionAndTime (Game.toTurn moves) coordinate time
                    in
                    case Board.play move (Game.toBoard moves boardSize) of
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
                    ( PlayingGame goban, Cmd.batch [ Download.string "game.sgf" "text/plain" (Game.toString sgf) ] )

                SelectGameFile ->
                    ( PlayingGame goban
                    , Cmd.batch
                        [ Select.file
                            [ -- "text/sgf"
                              -- ,
                              "text/plain"

                            -- , "text/*"
                            ]
                            GameFileSelected
                        ]
                    )

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

                ResetGame ( id, shouldReset ) ->
                    -- case D.decodeValue D.bool encodedBoolean of
                    -- Ok shouldReset ->
                    if shouldReset then
                        let
                            newGame =
                                Game.fresh goban.boardSize

                            newAppState =
                                updatePlayingGame goban newGame
                        in
                        ( PlayingGame newAppState, notify ( goban.currentGame, newGame ) )

                    else
                        -- _ ->
                        ( PlayingGame goban, Cmd.none )

                -- Err _ ->
                -- let
                --     aa =
                --         Debug.log "error" "yeees"
                -- in
                -- ( PlayingGame { goban | message = Just "Oh no not AGAIN!" }, Cmd.none )
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

                TurnBoard ->
                    let
                        nextTableTurn =
                            goban.tableTurn + 1
                    in
                    ( PlayingGame { goban | tableTurn = nextTableTurn }, Cmd.none )

                ChangeBoardSize ->
                    let
                        nextBoardSize =
                            case goban.boardSize of
                                BoardSized 9 ->
                                    BoardSized 13

                                BoardSized 13 ->
                                    BoardSized 19

                                BoardSized 19 ->
                                    BoardSized 9

                                _ ->
                                    BoardSized 19
                    in
                    ( PlayingGame { goban | boardSize = nextBoardSize }, Cmd.none )

                ShowMenu ->
                    ( PlayingGame { goban | showMenu = nextMenuState (0 < List.length moves) goban.showMenu }, Cmd.none )

                TriggerDelayedZoom ->
                    ( PlayingGame { goban | zoomState = TriggeredZoomIn }, Cmd.batch [ setTimer ZoomInOnBoard 2000 ] )

                ZoomInOut ->
                    let
                        nextZoomState =
                            if goban.zoomState == ZoomedOut then
                                ZoomedIn

                            else
                                ZoomedOut
                    in
                    ( PlayingGame { goban | zoomState = nextZoomState }, Cmd.none )

                ZoomInOnBoard ->
                    let
                        nextZoomState =
                            if goban.zoomState == TriggeredZoomIn then
                                ZoomedIn

                            else
                                ZoomedOut
                    in
                    ( PlayingGame { goban | zoomState = nextZoomState }, Cmd.none )

                ZoomOutOfBoard ->
                    ( PlayingGame { goban | zoomState = ZoomedIn }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : GobanApp -> Sub Msg
subscriptions _ =
    Sub.batch
        [ BrowserE.onResize (\_ _ -> Resize)
        , loadGame Load
        , resetGame ResetGame
        ]
