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
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Json.Decode as D
import Json.Encode as E
import List.Extra as ListExtra
import Move exposing (Move)
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
    { turn : Player
    , moves : List Move
    , table : Maybe Viewport
    , board : Board
    , message : Maybe String
    }


blankGame : Model
blankGame =
    { turn = Player.Black
    , moves = []
    , table = Nothing
    , board = Board.square 19
    , message = Nothing
    }


fromPlayerAndMoves : Player -> List Move -> Model
fromPlayerAndMoves p moves =
    { blankGame | turn = p, moves = moves, board = List.foldr Board.applyPlay (Board.square 19) moves }


init : E.Value -> ( Model, Cmd Msg )
init flags =
    ( case D.decodeValue decoder flags of
        Ok model ->
            model

        Err _ ->
            blankGame
    , Cmd.batch
        [ Task.attempt GotTable (Bdom.getViewportOf "table")
        ]
    )



-- JSON ENCODE/DECODE


encode : Model -> E.Value
encode model =
    E.object
        [ ( "turn", E.string (Player.toString model.turn) )
        , ( "moves", E.list Move.encode model.moves )
        ]


decoder : D.Decoder Model
decoder =
    D.map2
        fromPlayerAndMoves
        (D.field "turn" Player.decoder)
        (D.field "moves" (D.list Move.decoder))



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
                    , yunziis (Board.movesOf model.board)
                    , clickAreas
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
            , Element.text (Player.toString model.turn)
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
            Player.toString (Move.player move)

        xy =
            Move.position move
    in
    Svg.circle
        (List.append (onPosition xy)
            [ SvgA.r "5"
            , SvgA.fill color
            , SvgE.onClick (Click xy)
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click coordinate ->
            ( model, Time.now |> Task.perform (TimedClick coordinate) )

        TimedClick coordinate time ->
            let
                move =
                    Move.TimedMove model.turn coordinate time
            in
            case Board.play move model.board of
                Ok board ->
                    ( { model
                        | board = board
                        , turn = Player.next model.turn
                        , moves = move :: model.moves
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
            let
                oneLessMove =
                    Maybe.withDefault model.moves (List.tail model.moves)
            in
            ( { model
                | turn = Player.next model.turn
                , moves = oneLessMove
                , board = List.foldl Board.applyPlay (Board.square 19) oneLessMove
              }
            , Cmd.none
            )

        GotTable vp ->
            ( { model | table = Result.toMaybe vp }, Cmd.none )

        Resize ->
            ( model, Task.attempt GotTable (Bdom.getViewportOf "table") )


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
