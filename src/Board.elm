module Board exposing
    ( Board
    , InsertionFailure(..)
    , InsertionResult
    , applyPlay
    , capture
    , fromSizeStonesCaptures
    , get
    , groupAt
    , insertionFailureToString
    , isPositionOnBoard
    , movesOf
    , play
    , playMove
    , put
    , sizeOf
    , square
    , toString
    )

import Debug
import Dict exposing (Dict)
import Flip exposing (flip)
import Move exposing (Move(..))
import Player exposing (Player)
import Position exposing (Position)
import Result exposing (andThen)
import Set exposing (Set)


type alias InsertionResult =
    Result InsertionFailure Board


type InsertionFailure
    = Ko
    | Suicide
    | Occupied
    | OutOfBounds
    | Unspecified String


insertionFailureToString : InsertionFailure -> String
insertionFailureToString failure =
    case failure of
        Suicide ->
            "Suicidal mate?"

        Occupied ->
            "Social distancing please!"

        OutOfBounds ->
            "Don't throw yumnzis around."

        Unspecified _ ->
            "I dont undestand gibberisch!"

        Ko ->
            "Oh no, Deja vu!"


type alias Captures =
    ( List Position, List Position )


type alias Stones =
    Dict Position Player


type Board
    = Board
        { stones : Stones
        , captures : Captures
        , size : Int
        }


type alias Group =
    { positions : Set Position
    , player : Maybe Player
    , liberties : Set Position
    }


square : Int -> Board
square size =
    Board
        { stones = Dict.empty
        , captures = ( [], [] )
        , size = size
        }


fromSizeStonesCaptures : Int -> Stones -> Captures -> Board
fromSizeStonesCaptures size stones captures =
    Board { size = size, captures = captures, stones = stones }


sizeOf : Board -> Int
sizeOf (Board { size }) =
    size


movesOf : Board -> List Move
movesOf (Board { stones }) =
    Dict.foldl (\pos player list -> Move.fromPlayerAndPosition player pos :: list) [] stones



-- play a stone on the board following the rules (isFree, capture, isAlive, ...)


play : Move -> Board -> InsertionResult
play move board =
    isPositionOnBoard (Move.positionOf move) board
        |> andThen (\_ -> isFree board (Move.positionOf move))
        |> andThen (\_ -> capture board move)
        |> andThen (isAlive move)
        |> andThen Result.Ok


applyPlay : Move -> Board -> Board
applyPlay move board =
    case play move board of
        Err _ ->
            board

        Ok newBoard ->
            newBoard


playMove : Position -> Player -> Board -> InsertionResult
playMove pos player board =
    play (Move.fromPlayerAndPosition player pos) board


isPositionOnBoard : Position -> Board -> Result InsertionFailure ()
isPositionOnBoard pos (Board { size }) =
    if Position.isWithinSquare size pos then
        Result.Ok ()

    else
        Result.Err OutOfBounds


isFree : Board -> Position -> Result InsertionFailure ()
isFree (Board { stones }) coordinate =
    case Dict.get coordinate stones of
        Nothing ->
            Result.Ok ()

        Just _ ->
            Result.Err Occupied


capture : Board -> Move -> Result InsertionFailure Board
capture board move =
    let
        p =
            Move.playerOf move

        position =
            Move.positionOf move

        x =
            Position.x position

        y =
            Position.y position

        neighbors =
            [ ( x - 1, y )
            , ( x, y + 1 )
            , ( x + 1, y )
            , ( x, y - 1 )
            ]

        boardPutted =
            put (Move.fromPlayerAndPosition p (Position.fromXandY x y)) board

        groups =
            neighbors
                |> List.filter (isInBounds (sizeOf board))
                |> List.map (\pos -> groupAt pos boardPutted)

        captures =
            neighbors
                |> List.filter (isInBounds (sizeOf board))
                |> List.map (\pos -> groupAt pos boardPutted)
                |> List.filter
                    (\{ player } ->
                        case player of
                            Nothing ->
                                False

                            Just otherPlayer ->
                                otherPlayer /= p
                    )
                |> List.filter (\group -> Set.size group.liberties < 1)
    in
    case captures of
        [] ->
            Result.Ok boardPutted

        captures_ ->
            Result.Ok
                (List.foldl
                    (\cap board_ ->
                        List.foldl doCapture board_ (Set.toList cap.positions)
                    )
                    boardPutted
                    captures_
                )


isAlive : Move -> Board -> Result InsertionFailure Board
isAlive move board =
    let
        group =
            groupAt (Move.positionOf move) board
    in
    if Set.size group.liberties == 0 then
        Result.Err Suicide

    else
        Result.Ok board


get : Position -> Board -> Maybe Player
get pos (Board { stones }) =
    Dict.get pos stones



-- just put a stone on the board


put : Move -> Board -> Board
put move (Board board) =
    Board { board | stones = Dict.insert (Move.positionOf move) (Move.playerOf move) board.stones }



-- adds captures for a player


captureAdd : Position -> Player -> Captures -> Captures
captureAdd pos player cap =
    case player of
        Player.White ->
            ( Tuple.first cap, pos :: Tuple.second cap )

        Player.Black ->
            ( pos :: Tuple.first cap, Tuple.second cap )


doCapture : Position -> Board -> Board
doCapture position (Board board) =
    let
        gotCapturedPlayer =
            get position (Board board)
    in
    case gotCapturedPlayer of
        Nothing ->
            Board board

        Just player ->
            Board
                { stones = Dict.remove position board.stones
                , size = board.size
                , captures = captureAdd position (Player.next player) board.captures
                }


emptyGroup : Group
emptyGroup =
    { positions = Set.empty, player = Nothing, liberties = Set.empty }


groupAt : Position -> Board -> Group
groupAt pos board =
    case get pos board of
        Nothing ->
            emptyGroup

        Just player ->
            groupRec
                pos
                board
                { player = Just player
                , liberties = Set.empty
                , positions = Set.fromList [ pos ]
                }


groupRec : Position -> Board -> Group -> Group
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
                |> List.filter (isInBounds (sizeOf board))
                |> List.filter (not << flip Set.member group.positions)

        checker position g =
            case get position board of
                Nothing ->
                    -- liberty
                    { g | liberties = Set.insert position g.liberties }

                Just player ->
                    if Just player == g.player then
                        -- extend group
                        groupRec
                            position
                            board
                            { g
                                | positions = Set.insert position g.positions
                            }

                    else
                        -- edge of group
                        g
    in
    List.foldl checker group toCheck


isInBounds : Int -> Position -> Bool
isInBounds size position =
    Position.isWithinSquare size position


toString : Board -> String
toString (Board { stones, size }) =
    let
        range =
            List.range 1 size

        row : Int -> String
        row y =
            range
                |> List.map
                    (\x ->
                        case Dict.get ( x, y ) stones of
                            Nothing ->
                                '-'

                            Just player ->
                                Player.toChar player
                    )
                |> String.fromList
                |> flip (++) "\n"

        rows : String
        rows =
            List.foldr ((++) << row) "" range
    in
    String.fromInt size ++ "\n" ++ rows
