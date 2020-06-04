module Main exposing (main)

-- import Element.html as ElHtml
-- import Json.Decode as Decode

import Basics
import Board exposing (Board)
import Browser
import Browser.Dom as Bdom exposing (Viewport)
import Browser.Events as BrowserE
import Coordinate exposing (Coordinate)
import Debug
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import List.Extra as ListExtra
import Move exposing (Move)
import Platform.Cmd
import Player exposing (Player)
import Svg exposing (Svg)
import Svg.Attributes as SvgA
import Svg.Events as SvgE
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { turn : Player
    , moves : List Move
    , table : Maybe Viewport
    }


init : string -> ( Model, Platform.Cmd.Cmd Msg )
init _ =
    ( { turn = Player.black
      , moves = []
      , table = Nothing
      }
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
            , Element.centerX
            , Element.htmlAttribute (HtmlA.id "table")
            ]
            (board model)
        , Element.el
            [ Element.width (Element.fillPortion 1)
            ]
            (sideBoard model)
        ]


board : Model -> Element Msg
board model =
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
                    , yunziis model.moves
                    , clickAreas
                    ]
                )
            )
        )


sideBoard : Model -> Element Msg
sideBoard model =
    Element.el
        [ Element.width (Element.fillPortion 3) ]
        (Element.column []
            (Element.el [] buttons
                :: List.map
                    Element.text
                    (Player.toString model.turn :: List.map Move.toString model.moves)
            )
        )


buttons : Element Msg
buttons =
    Element.row []
        [ Element.html (Html.button [ HtmlE.onClick Pass ] [ Html.text "pass" ])
        , Element.html (Html.button [ HtmlE.onClick Undo ] [ Html.text "undo" ])
        ]


yunziis : List Move -> Svg Msg
yunziis moves =
    Svg.g [] (List.map yunzi moves)


onCoordinate : Coordinate -> List (Svg.Attribute Msg)
onCoordinate pos =
    [ SvgA.cx (str (fieldSize * Coordinate.x pos))
    , SvgA.cy (str (fieldSize * Coordinate.y pos))
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
        (List.append (onCoordinate xy)
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


clicky : Coordinate -> Svg Msg
clicky xy =
    Svg.circle
        (List.append (onCoordinate xy)
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
        [ SvgA.r "2"
        , SvgA.cx (str (fieldSize * x))
        , SvgA.cy (str (fieldSize * y))
        , SvgA.fill "black"
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
    Svg.line (List.append (rowAt idx) [ SvgA.strokeWidth "1", SvgA.stroke "black" ]) []


colAt : Int -> List (Svg.Attribute Msg)
colAt x =
    [ SvgA.x1 (str (fieldSize * x))
    , SvgA.x2 (str (fieldSize * x))
    , SvgA.y1 (str fieldStart)
    , SvgA.y2 (str (fieldSize * 19))
    ]


colLine : Int -> Svg Msg
colLine idx =
    Svg.line (List.append (colAt idx) [ SvgA.strokeWidth "1", SvgA.stroke "black" ]) []



-- UPDATE


type Msg
    = Click Coordinate
    | Pass
    | Undo
    | GotTable (Result Bdom.Error Viewport)
    | Resize


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click coordinate ->
            ( { model
                | turn = Player.next model.turn
                , moves = Move.fromPlayerAndCoordinate model.turn coordinate :: model.moves
              }
            , Cmd.none
            )

        Pass ->
            ( { model | turn = Player.next model.turn }, Cmd.none )

        Undo ->
            ( { model
                | turn = Player.next model.turn
                , moves = Maybe.withDefault model.moves (ListExtra.init model.moves)
              }
            , Cmd.none
            )

        GotTable vp ->
            ( { model | table = Result.toMaybe vp }, Cmd.none )

        Resize ->
            ( model, Task.attempt GotTable (Bdom.getViewportOf "table") )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    BrowserE.onResize (\_ _ -> Resize)
