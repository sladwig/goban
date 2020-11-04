port module Main exposing (main)

-- import Element.html as ElHtml
-- import Json.Decode as Decode
-- import Element.Nothing as Nothing

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
        , update = updateWithStorage
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { game : Game
    , table : Maybe Viewport
    , message : Maybe String
    , editing : Maybe Position
    }


blank : Model
blank =
    { game = Game.fresh
    , table = Nothing
    , message = Nothing
    , editing = Nothing
    }




init : E.Value -> ( Model, Cmd Msg )
init flags =
    ( case D.decodeValue Game.decoder flags of
        Ok game ->
            { blank | game = game }

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


view : Model -> Html Msg
view model =
    Element.layout
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
        (boardSideBoard model)


boardSideBoard : Model -> Element Msg
boardSideBoard model =
    Element.row
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.centerX
        , Element.centerY
        , Element.spacing 30
        ]
        [ Element.el
            [ Element.width (Element.fillPortion 3)
            , Element.height Element.fill

            -- , Background.color (Element.rgb255 240 0 245)
            , Element.centerX
            , Element.htmlAttribute (HtmlA.id "table")
            ]
            (prettyBoard model)
        , Element.el
            [ Element.width (Element.fillPortion 1)
            ]
            (sideBoard model)
        ]


prettyBoard : Model -> Element Msg
prettyBoard model =
    let
        theSize =
            str (20 * fieldSize)

        svgSize =
            case model.table of
                Nothing ->
                    0

                Just table ->
                    Basics.min table.viewport.height table.viewport.width
    in
    Element.el
        [ Element.width (Element.fillPortion 3)
        , Element.height Element.fill
        ]
        (Element.el
            [ Element.width (Element.px (Basics.ceiling svgSize))
            , Element.centerX
            , Element.centerY
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
                    , clickAreas
                    , yunziis (Board.movesOf (Game.toBoard model.game.moves))
                    ]
                )
            )
        )


sideBoard : Model -> Element Msg
sideBoard model =
    Element.el
        [ Element.width (Element.fillPortion 3) ]
        (Element.column [ Element.scrollbars ]
            [ --message model.message
              Element.el [] buttons
            , Element.text (Player.toString (Game.toTurn model.game.moves))
            , viewEditing model.editing
            , Input.multiline
                [ Element.height (Element.px 400)
                ]
                { label = Input.labelHidden "jo"
                , onChange = \_ -> Undo
                , placeholder = Just (Input.placeholder [] (Element.text ""))
                , spellcheck = False
                , text = String.join "\n" (List.map Move.toString model.moves)
                }
            ]
        )


viewEditing : Maybe Position -> Element Msg
viewEditing p =
    case p of
        Nothing ->
            Element.text ""

        Just pos ->
            Element.text (Position.toString pos)


message : Maybe String -> Element Msg
message msg =
    case msg of
        Nothing ->
            Element.none

        Just reason ->
            Element.text reason


buttons : Element Msg
buttons =
    Element.row []
        [ Element.html (Html.button [ HtmlE.onClick Pass ] [ Html.text "pass" ])
        , Element.html (Html.button [ HtmlE.onClick Undo ] [ Html.text "undo" ])
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
            , SvgE.onClick (EditStone pos)
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
            , SvgE.onClick (Click xy)
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
dot x y =
    Svg.circle
        [ SvgA.r "1.5"
        , SvgA.cx (str (fieldSize * x))
        , SvgA.cy (str (fieldSize * y))
        , SvgA.fill "#2d2d2d"
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


port setStorage : E.Value -> Cmd msg



-- UPDATE


type Msg
    = Click Position
    | TimedClick Position Time.Posix
    | Pass
    | Undo
    | GotTable (Result Bdom.Error Viewport)
    | Resize
    | EditStone Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        moves =
            model.game.moves
    in
    case msg of
        Click coordinate ->
            case model.editing of
                Nothing ->
                    ( model, Time.now |> Task.perform (TimedClick coordinate) )

                Just pos ->
                    let
                        elementIndex =
                            ListExtra.findIndex
                                (\move ->
                                    let
                                        movepos =
                                            Move.positionOf move
                                    in
                                    Position.x movepos == Position.x pos && Position.y movepos == Position.y pos
                                )
                                moves

                        replacedMove : Maybe Move
                        replacedMove =
                            case elementIndex of
                                Just i ->
                                    ListExtra.getAt i moves

                                Nothing ->
                                    Nothing

                        newMoves =
                            case ( elementIndex, replacedMove ) of
                                ( Just i, Just oldMove ) ->
                                    case oldMove of
                                        Move.Normal a ->
                                            ListExtra.setAt i (Move.fromPlayerAndPosition (Move.playerOf oldMove) coordinate) moves

                                        Move.Timed a ->
                                            ListExtra.setAt i (Move.fromPlayerAndPositionAndTime (Move.playerOf oldMove) coordinate a.at) moves

                                _ ->
                                    moves
                    in
                    ( { model | game = Game.fromMoves newMoves, editing = Nothing }, Cmd.none )

        TimedClick coordinate time ->
            let
                move =
                    Move.fromPlayerAndPositionAndTime (Game.toTurn moves) coordinate time
            in
            case Board.play move (Game.toBoard moves) of
                Ok board ->
                    ( { model
                        | game = Game.makeMove model.game move
                        , message = Nothing
                      }
                    , Cmd.none
                    )

                Err reason ->
                    ( { model
                        | message = Just (Board.insertionFailureToString reason)
                      }
                    , Cmd.none
                    )

        Pass ->
            ( { model | turn = Player.next model.turn }, Cmd.none )

        Undo ->
            ( { model | game = Game.undoMove model.game }, Cmd.none )

        GotTable vp ->
            ( { model | table = Result.toMaybe vp }, Cmd.none )

        Resize ->
            ( model, Task.attempt GotTable (Bdom.getViewportOf "table") )

        EditStone position ->
            ( { model | editing = Just position }, Cmd.none )


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg oldModel =
    let
        ( newModel, cmds ) =
            update msg oldModel
    in
    ( newModel
    , Cmd.batch [ setStorage (encode newModel), cmds ]
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    BrowserE.onResize (\_ _ -> Resize)
