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
import Move exposing (Move)
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
            "suicidal?"

        Occupied ->
            "social distancing please!"

        OutOfBounds ->
            "be realistic!"

        Unspecified _ ->
            "I dont undestand gibberisch!"

        Ko ->
            "oh no"


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



-- allowed : Move -> Board -> Bool
-- allowed move (Board board) =
--     isOnBoard (Move.position move)
--         |> andThen (\_ -> isFree (Board board) (Move.position move))
--         |> andThen (\_ -> isAlive (Board board) move)
-- play a stone on the board following the rules (isFree, capture, isAlive, ...)


play : Move -> Board -> InsertionResult
play move board =
    isPositionOnBoard (Move.position move) board
        |> andThen (\_ -> isFree board (Move.position move))
        |> andThen (\_ -> capture board move)
        |> andThen (isAlive move)
        |> andThen (\iiii -> Result.Ok (Debug.log "iiii" iiii))


applyPlay : Move -> Board -> Board
applyPlay move board =
    case play move board of
        Err _ ->
            board

        Ok newBoard ->
            newBoard


playMove : Position -> Player -> Board -> InsertionResult
playMove pos player board =
    play (Move.fromPositionAndPlayer pos player) board



-- |> andThen (\tmpBoard -> isAlive (Board tmpBoard) move)
-- |> andThen (\tmpBoard -> put move (Board tmpBoard) |> Result.Ok)


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



-- capture (Board { stones, captures })


capture board ( player, ( x, y ) ) =
    let
        neighbors =
            [ ( x - 1, y )
            , ( x, y + 1 )
            , ( x + 1, y )
            , ( x, y - 1 )
            ]

        boardPutted =
            Debug.log "bbboard" (put (Move.fromPositionAndPlayer ( x, y ) player) board)

        groups =
            Debug.log ("groupd" ++ Position.toString ( x, y ))
                (neighbors
                    |> List.filter (isInBounds (sizeOf board))
                    -- |> List.map (\pos -> put (Move.fromPositionAndPlayer (x, y) player) board)
                    |> List.map (\pos -> groupAt pos boardPutted)
                )

        captures =
            neighbors
                |> List.filter (isInBounds (sizeOf board))
                -- |> List.map (\pos -> put (Move.fromPositionAndPlayer (x, y) player) board)
                |> List.map (\pos -> groupAt pos boardPutted)
                |> List.filter
                    (\group ->
                        case Debug.log "group" group.player of
                            Nothing ->
                                False

                            Just otherPlayer ->
                                Debug.log "other" otherPlayer /= Debug.log "plaer :" player
                    )
                |> List.filter (\group -> Set.size group.liberties < 1)

        -- |> Debug.log
        --  (List.filter isInBounds)
        -- |> List.filter grou (Move.player move)
        -- |> List.filter (not << flip Set.member group.coordinates)
    in
    case Debug.log "CAPTURE" captures of
        [] ->
            Result.Ok boardPutted

        captures_ ->
            Result.Ok
                (Debug.log "FINAL"
                    (List.foldl
                        (\cap board_ ->
                            List.foldl (\pos board__ -> doCapture pos board__) board_ (Set.toList cap.positions)
                        )
                        boardPutted
                        captures_
                    )
                )



-- Nothing ->
--     Result.Ok (Board { stones = stones, captures = captures })
-- Just _ ->
--     Result.Err Occupied


isAlive : Move -> Board -> Result InsertionFailure Board
isAlive move board =
    let
        -- inserted =
        --     put move board
        group =
            groupAt (Move.position move) board
    in
    if Set.size group.liberties == 0 then
        Result.Err Suicide

    else
        Result.Ok board



-- map2 (,) (index 0 string) (index 1 string)


get : Position -> Board -> Maybe Player
get pos (Board { stones }) =
    Dict.get pos (Debug.log "stpmu" stones)



-- just put a stone on the board


put : Move -> Board -> Board
put move (Board board) =
    Board { board | stones = Dict.insert (Move.position move) (Move.player move) board.stones }


type alias Group =
    { positions : Set Position
    , player : Maybe Player
    , liberties : Set Position
    }



-- type alias EmptyGroup =
--     { positions : Set Position
--     , player : Maybe.Nothing
--     , liberties : Set Position
--     }
-- type Group
--     = PlayerGroup
--     | EmptyGroup


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



--     , captures = {Set.insert position (Dict.get (Player.nextPlayer gotCapturedPlayer))
-- }


emptyGroup : Group
emptyGroup =
    { positions = Set.empty, player = Nothing, liberties = Set.empty }


groupAt : Position -> Board -> Group
groupAt pos board =
    case Debug.log ("GROUPAT " ++ Position.toString pos) (get pos board) of
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
            Debug.log ("groupRecNeighbord" ++ Position.toString ( x, y ))
                [ ( x + 1, y )
                , ( x - 1, y )
                , ( x, y - 1 )
                , ( x, y + 1 )
                ]

        toCheck =
            Debug.log "groupRecToCheck"
                (neighbors
                    |> List.filter (isInBounds (sizeOf board))
                    |> List.filter (not << flip Set.member group.positions)
                )

        checker position g =
            case Debug.log ("ohhhyu" ++ Position.toString position) (get position board) of
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

                                -- , player = Just g.player
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
