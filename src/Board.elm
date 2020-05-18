module Board
    exposing
        ( Board
        , InsertionResult
        , InsertionFailure(..)
        , insert
        , get
        , set
        , sizeOf
        , square
        , toString
        )

import Result exposing (andThen)
import Dict exposing (Dict)
import Set exposing (Set)
import Player exposing (Player)
import Coordinate exposing (Coordinate)
import Flip exposing (flip)

import Debug 

type alias InsertionResult =
    Result InsertionFailure Board


type InsertionFailure
    = Ko
    | Suicide
    | Occupied
    | OutOfBounds
    | Unspecified String


type Board
    = Board
        { dict : Dict Coordinate Player
        , size : Int
        }


type alias Group =
    { coordinates : Set Coordinate
    , player : Player
    , liberties : Set Coordinate
    }


square : Int -> Board
square size =
    Board { dict = Dict.empty, size = size }


sizeOf : Board -> Int
sizeOf (Board { size }) =
    size


insert : Coordinate -> Player -> Board -> InsertionResult
insert coordinate player (Board board) =
    validateCoordinate coordinate (Board board)
        |> andThen (\_ -> validateAvailable board.dict coordinate)
        |> andThen (\_ -> validateLiberties (Board board) coordinate player)
        |> andThen (\_ -> set coordinate player (Board board) |> Result.Ok)


validateCoordinate : Coordinate -> Board -> Result InsertionFailure ()
validateCoordinate coordinate board =
    if isInBounds board coordinate then
        Result.Ok ()
    else
        Result.Err OutOfBounds


validateAvailable : Dict Coordinate Player -> Coordinate -> Result InsertionFailure ()
validateAvailable dict coordinate =
    case Dict.get coordinate dict of
        Nothing ->
            Result.Ok ()

        Just _ ->
            Result.Err Occupied


validateLiberties : Board -> Coordinate -> Player -> Result InsertionFailure ()
validateLiberties board coordinate player =
    let
        inserted =
            set coordinate player board
    in
        case groupAt coordinate inserted of
            Nothing ->
                Result.Err <| Unspecified ((Debug.toString coordinate) ++ "not in a group")

            Just group_ ->
                if Set.size (group_.liberties) == 0 then
                    Result.Err Suicide
                else
                    Result.Ok ()

-- map2 (,) (index 0 string) (index 1 string)

get : Coordinate -> Board -> Maybe Player
get coordinate (Board { dict }) =
    Dict.get coordinate dict


set : Coordinate -> Player -> Board -> Board
set coordinate player (Board board) =
    Board { board | dict = Dict.insert coordinate player board.dict }


groupAt : Coordinate -> Board -> Maybe Group
groupAt coordinate board =
    get coordinate board
        |> Maybe.map
            (\player ->
                groupRec coordinate
                    board
                    { player = player
                    , liberties = Set.empty
                    , coordinates = Set.empty
                    }
            )


groupRec : Coordinate -> Board -> Group -> Group
groupRec ( x, y ) board group =
    let
        neighbors =
            [ ( x + 1, y )
            , ( x - 1, y )
            , ( x, y - 1 )
            , ( x, y + 1 )
            ]

        toCheck =
            neighbors
                |> List.filter (isInBounds board)
                |> List.filter (not << flip Set.member group.coordinates)

        checker coordinate g =
            case get coordinate board of
                Nothing ->
                    -- liberty
                    { g | liberties = Set.insert coordinate g.liberties }

                Just player ->
                    if player == g.player then
                        -- extend group
                        groupRec
                            coordinate
                            board
                            { g
                                | coordinates = Set.insert coordinate g.coordinates
                            }
                    else
                        -- edge of group
                        g
    in
        List.foldl checker group toCheck


isInBounds : Board -> Coordinate -> Bool
isInBounds (Board { size }) coordinate =
    Coordinate.isWithinSquare size coordinate


-- toString_ =
--     Basics.toString


toString : Board -> String
toString (Board { size, dict }) =
    let
        range =
            List.range 1 size

        row : Int -> String
        row y =
            range
                |> List.map
                    (\x ->
                        case Dict.get ( x, y ) dict of
                            Nothing ->
                                '-'

                            Just player ->
                                Player.toChar player
                    )
                |> String.fromList
                |> (flip (++) "\n")

        rows : String
        rows =
            List.foldr ((++) << row) "" range
    in
        (String.fromInt size) ++ "\n" ++ rows
