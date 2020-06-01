module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Events exposing (onClick)
import Board exposing (Board)
import Player exposing (Player)
import Coordinate exposing (Coordinate)
import Debug
import Platform.Cmd
import Flip exposing (flip)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import List.Extra exposing (init)

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
    {
        -- board : Board
 turn : Player
    , moves: List (Coordinate, Player)
    , message : Maybe String
    }


init : string -> ( Model, Platform.Cmd.Cmd Msg )
init flags =
    ({ -- board = Board.square 19
    turn = Player.black
    , moves = []
    , message = Nothing
    }, Cmd.none)




-- VIEW
fieldSize = 10
str = String.fromInt

view : Model -> Html Msg
view model =
    let
        size = 19
            -- Board.sizeOf model.board

        messages =
            case model.message of
                Nothing ->
                    []

                Just message ->
                    [ Html.div [] [ Html.text message ]
                    ]
    in
        Html.div []
            [
                        Html.button [ onClick Pass ] [ Html.text "pass" ]
                , Html.button [ onClick Undo ] [ Html.text "undo" ]


             ,svg [ version "1.1", x "0", y "0", scale "1", viewBox "0 0 323 323", enableBackground "green" ] [
                --  rect [ x "5", y "5", fill "beige",  width (str (21 * fieldSize)), height (str (21 * fieldSize))] []
                 svgRows
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
                , (yunziis model.moves)
                , clickAreas
             ],
                       stateShow model





        --      ++ (List.map (viewRow model.board size)
        --                 (List.range 1 size)
        -- )

        -- (List.map (viewRow model.board size)
        --                 (List.range 1 size)
        -- )

                -- ++ messages
              ]

stateShow : Model -> Html Msg
stateShow model = Html.div [] ([
    case model.turn of
        Player.White -> Html.text "white"
        Player.Black -> Html.text "black"
    ] ++ (List.map moved model.moves))

moved : (Coordinate, Player) -> Html Msg
moved (xy, player) =
    let
        (x, y) = (Tuple.mapBoth toText toText xy)
    in
    Html.span [] [
        case player of
        Player.White -> Html.text "white ("
        Player.Black -> Html.text "black ("
    , x, Html.text " | ", y, Html.text ") --- " ]

-- ]
toText : Int -> Html Msg
toText n = Html.text (String.fromInt n)

yunziis : List (Coordinate, Player) -> Svg Msg
yunziis moves =
    g [] (List.map yunzi moves)

yunzi : (Coordinate, Player) -> Svg Msg
yunzi (xy, player) =
    let
        color = (case player of
                    Player.Black -> "black"
                    Player.White -> "white")
    in
    circle [ r "5", cx (str (fieldSize * (Tuple.first xy))), cy (str (fieldSize * (Tuple.second xy))), fill color, onClick (Click xy)] []

clickAreas : Svg Msg
clickAreas =
     g [](List.map hoplas (List.range 1 19))

hoplas : Int->Svg Msg
hoplas row =
    g [] (List.map clicky (List.map (Tuple.pair row) (List.range 1 19)))


clicky xy = circle [ r "5", opacity "0.5", cx (str (fieldSize * (Tuple.first xy))), cy (str (fieldSize * (Tuple.second xy))), fill "transparent", onClick (Click xy)] []

svgRows : Svg Msg
svgRows =
    g [](List.map rowLine (List.range 1 19))

svgCols : Svg Msg
svgCols =
    g [](List.map colLine (List.range 1 19))

        -- rowLine index
        -- line [x (str fieldSize), y (str fieldSize)] []

dot: Int -> Int -> Svg Msg
dot x y =
    circle [ r "2", cx (str (fieldSize * x)), cy (str (fieldSize * y)), fill "black"] []

rowLine :  Int  -> Svg Msg
rowLine idx =
    line [x1 (str (fieldSize*idx)), x2 (str (fieldSize*idx)), y1  (str fieldSize), y2 (str (fieldSize*19)), strokeWidth "1", stroke "black"] []

colLine :  Int  -> Svg Msg
colLine idx =
    line [y1 (str (fieldSize*idx)), y2 (str (fieldSize*idx)), x1  (str fieldSize), x2 (str (fieldSize*19)), strokeWidth "1", stroke "black"] []

-- svgLineRow : Board -> Int -> Int -> Html Msg
-- svgLineRow board xSize y =
--     let
--         range =
--             List.map (flip Coordinate.fromXandY y) (List.range 1 xSize)
--     in
--         Html.tr [] (List.map (viewCell board) range)


viewRow : Board -> Int -> Int -> Html Msg
viewRow board xSize y =
    let
        range =
            List.map (flip Coordinate.fromXandY y) (List.range 1 xSize)
    in
        Html.tr [] (List.map (viewCell board) range)


viewCell : Board -> Coordinate -> Html Msg
viewCell board coordinate =
    let
        occupant =
            Board.get coordinate board

        rendered =
            case occupant of
                Just player ->
                    Html.text <| (Debug.toString player)

                Nothing ->
                    Html.em []
                        [ Html.text <| (Debug.toString coordinate)
                        ]
    in
        Html.td [ onClick (Click coordinate) ] [ rendered ]



-- UPDATE


type Msg
    = Click Coordinate
    | Pass | Undo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click coordinate ->
            -- case Board.insert coordinate model.turn model.board of
                -- Ok board ->
                    ({ model |
                        -- | board = board

                        turn = Player.next model.turn
                        , moves = model.moves ++ [(coordinate, model.turn)]
                        , message = Nothing
                    }, Cmd.none)

                -- Err reason ->
                --     ({ model | message = Just <| (Debug.toString reason) }, Cmd.none)

        Pass ->
            ({ model | turn = Player.next model.turn }, Cmd.none)
        Undo ->
            ({ model | turn = Player.next model.turn,  moves = (Maybe.withDefault model.moves (List.Extra.init model.moves)) }, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none
