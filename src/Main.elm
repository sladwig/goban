module Main exposing (main)

import Board exposing (Board)
import Browser
import Coordinate exposing (Coordinate)
import Debug
import Html exposing (Html)
import Html.Events exposing (onClick)
import List.Extra exposing (init)
import Move exposing (Move)
import Platform.Cmd
import Player exposing (Player)
import Svg exposing (..)
import Svg.Attributes exposing (..)


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
    { -- board : Board
      turn : Player
    , moves : List Move

    -- , message : Maybe String
    }


init : string -> ( Model, Platform.Cmd.Cmd Msg )
init _ =
    ( { turn = Player.black
      , moves = []
      }
    , Cmd.none
    )



-- VIEW


fieldSize : Int
fieldSize =
    10


str : Int -> String
str =
    String.fromInt


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.button [ onClick Pass ] [ Html.text "pass" ]
        , Html.button [ onClick Undo ] [ Html.text "undo" ]
        , svg [ version "1.1", x "0", y "0", scale "1", viewBox "0 0 323 323", enableBackground "green" ]
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
        , stateShow model
        ]


stateShow : Model -> Html Msg
stateShow model =
    Html.div [] (List.map Html.text (Player.toString model.turn :: List.map Move.toString model.moves))


yunziis : List Move -> Svg Msg
yunziis moves =
    g [] (List.map yunzi moves)


onCoordinate : Coordinate -> List (Attribute Msg)
onCoordinate pos =
    [ cx (str (fieldSize * Coordinate.x pos)), cy (str (fieldSize * Coordinate.y pos)) ]


yunzi : Move -> Svg Msg
yunzi move =
    let
        color =
            Player.toString (Move.player move)

        xy =
            Move.position move
    in
    circle (List.append (onCoordinate xy) [ r "5", fill color, onClick (Click xy) ]) []


clickAreas : Svg Msg
clickAreas =
    g [] (List.map hoplas (List.range 1 19))


hoplas : Int -> Svg Msg
hoplas row =
    g [] (List.map clicky (List.map (Tuple.pair row) (List.range 1 19)))


clicky : Coordinate -> Svg Msg
clicky xy =
    circle (List.append (onCoordinate xy) [ r "5", opacity "0.5", fill "transparent", onClick (Click xy) ]) []


svgRows : Svg Msg
svgRows =
    g [] (List.map rowLine (List.range 1 19))


svgCols : Svg Msg
svgCols =
    g [] (List.map colLine (List.range 1 19))


dot : Int -> Int -> Svg Msg
dot x y =
    circle [ r "2", cx (str (fieldSize * x)), cy (str (fieldSize * y)), fill "black" ] []


rowAt : Int -> List (Attribute Msg)
rowAt x =
    [ x1 (str (fieldSize * x))
    , x2 (str (fieldSize * x))
    , y1 (str fieldSize)
    , y2 (str (fieldSize * 19))
    ]


rowLine : Int -> Svg Msg
rowLine idx =
    line (List.append (rowAt idx) [ strokeWidth "1", stroke "black" ]) []


colAt : Int -> List (Attribute Msg)
colAt x =
    [ y1 (str (fieldSize * x))
    , y2 (str (fieldSize * x))
    , x1 (str fieldSize)
    , x2 (str (fieldSize * 19))
    ]


colLine : Int -> Svg Msg
colLine idx =
    line (List.append (colAt idx) [ strokeWidth "1", stroke "black" ]) []



-- UPDATE


type Msg
    = Click Coordinate
    | Pass
    | Undo


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
            ( { model | turn = Player.next model.turn, moves = Maybe.withDefault model.moves (List.Extra.init model.moves) }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none
