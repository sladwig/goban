module GameTests exposing (..)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, constant, int, intRange, list, string)
import Game exposing (Game)
import Move exposing (Move)
import Parser exposing (Parser)
import Player exposing (Player(..))
import Set exposing (Set)
import Test exposing (..)


someGame : String
someGame =
    """
(;GM[1]FF[4]CA[UTF-8]AP[Sabaki:0.51.1]KM[6.5]SZ[19]DT[2020-05-17]PB[Olla]PW[Dolla]WR[high]GN[#1]GC[23:00 POG "The enemy's keypoint is yours"];B[oc];W[co];B[ec];W[po];B[jc];W[qi];B[qf];W[jp];B[ci];W[dk];B[cg];W[og];B[mg];W[nf];B[oi];W[ph];B[me];W[mh];B[nh];W[ng];B[lh];W[mi];B[oh];W[mf];B[lf];W[lg];B[pg];W[kf];B[le];W[kh];B[of];W[ne];B[nd];W[oe];B[pf];W[pd];B[qc];W[qd];B[rd];W[rf];B[re];W[qe];B[rg];W[qh];B[sf];W[rh];B[rf];W[rc];B[sc];W[pc];B[rb];W[pb];B[ob];W[qb];B[ra];W[rc];B[od];W[qc];B[pe];W[md];B[mg];W[li];B[nf];W[pj];B[oj];W[ok];B[pk];W[qk];B[pl];W[nk];B[ql];W[rk];B[pi];W[qj];B[mj];W[nj];B[mk];W[nl];B[nm];W[ml];B[pm];W[lk];B[rl];W[sl];B[rn];W[sm];B[sn];W[rm];B[qm];W[sk];B[pn];W[qo];B[qn];W[ro];B[sh];W[si];B[sg];W[rj];B[om];W[mm];B[oo];W[op];B[no])
"""


anotherGame : String
anotherGame =
    """
(;G[1]FF[4]CA[UTF-8]KM[0]SZ[19]DT[_the time_]BP[_player black_]BR[_black rank_]WP[_player white_]WR[_white rank_]GN[_Game Name_]GC[_Game Comment_];B[pd]DT[1605060454485];W[pp]DT[1605060455667];B[dp]DT[1605060456882];W[dd]DT[1605060457928];B[mh]DT[1605061576332];W[ke]DT[1605061577029];B[jh]DT[1605061578762];W[jg]DT[1605061579246];B[gf]DT[1605061579717];W[fl]DT[1605061580988];B[ea]DT[1605061582991];W[lk]DT[1605064005007];B[ll]DT[1605064005220];W[hp]DT[1605064005831];B[il]DT[1605064006335];W[og]DT[1605064009765];B[ne]DT[1605064010374];W[lb]DT[1605064010914];B[nc]DT[1605064012455];W[ob]DT[1605064012995];B[pk]DT[1605064014007];W[nm]DT[1605064015166];B[mq]DT[1605064015920];W[ko]DT[1605064016595];B[gh]DT[1605064018384];W[fg]DT[1605064018889];B[ef]DT[1605064019633];W[cg]DT[1605064020443];B[bi]DT[1605064021635];W[ck]DT[1605064022355];B[bo]DT[1605064022850];W[bp]DT[1605064023390];B[do]DT[1605064023885];W[fm]DT[1605064024267];B[jl]DT[1605064025021];W[kl]DT[1605064025291];B[lj]DT[1605064025662];W[oh]DT[1605064026416];B[qh]DT[1605064026652];W[qe]DT[1605064027406];B[pb]DT[1605064028239];W[pa]DT[1605064028756];B[qa]DT[1605064029049];W[qb]DT[1605064029748];B[rd]DT[1605064031400];W[rc]DT[1605064032065];B[rb]DT[1605064032874];W[ra]DT[1605064033402];B[gd]DT[1605064035360];W[gc]DT[1605064036024];B[ec]DT[1605064036440];W[ee]DT[1605064037002];B[he]DT[1605064037857];W[ie]DT[1605064038105];B[hg]DT[1605064038825];W[ij]DT[1605064039601];B[jj]DT[1605064040200];W[kj]DT[1605064040512];B[ol]DT[1605064041998];W[pn]DT[1605064042695];B[oo]DT[1605064043302];W[kn]DT[1605064044911];B[jn]DT[1605064045867];W[ld]DT[1605065794546];B[ka]DT[1605065804760];W[ia]DT[1605087642372];B[fa]DT[1605094177089];W[bc]DT[1605094180598];B[gi]DT[1605099962708];W[ni]DT[1605102768057];B[lg]DT[1605103765812];W[lf]DT[1605103767106];B[lh]DT[1605103929747];W[pg]DT[1605103945048];B[pf]DT[1605103946454];W[oi]DT[1605104139696];B[nf]DT[1605104263807];W[mf]DT[1605104265911];B[pe]DT[1605108544341];W[qa]DT[1605108547536];B[jk]DT[1605110912989])"""


aGameWithMoves : Game
aGameWithMoves =
    let
        a =
            Game.fresh
    in
    { a | moves = [ Move.fromPlayerAndPosition Player.Black ( 1, 8 ) ] }


suite : Test
suite =
    describe "Game"
        [ describe "toSgf"
            [ test "fresh game" <|
                \_ ->
                    Expect.equal (Game.toSgf Game.fresh) ("(;" ++ String.concat (List.map Game.infoToSgf Game.fresh.infos) ++ ")")
            , test "Game with moves" <|
                \_ ->
                    let
                        sgf =
                            Game.toSgf aGameWithMoves

                        gameInfos =
                            String.concat (List.map Game.infoToSgf Game.fresh.infos)

                        moves =
                            String.concat (List.map Move.toSgf aGameWithMoves.moves)

                        expectedSgf =
                            "(;" ++ gameInfos ++ ";" ++ moves ++ ")"
                    in
                    Expect.equal sgf expectedSgf
            ]
        , describe "fromSgf"
            [ test "normal game" <|
                \_ ->
                    Expect.equal (Parser.run Game.fromSgf (Game.toSgf aGameWithMoves)) (Result.Ok aGameWithMoves)
            , test "some game" <|
                \_ ->
                    Expect.ok (Parser.run Game.fromSgf someGame)
            , test "another game" <|
                \_ ->
                    Expect.ok (Parser.run Game.fromSgf anotherGame)
            ]
        , describe "Game Arguments"
            [ test "can be parsed" <|
                \_ ->
                    Expect.equal (Parser.run Game.gameInfoParser "G[jo]") (Result.Ok <| Game.info "G" "jo")
            , test "can be parsed with two Letters" <|
                \_ ->
                    Expect.equal (Parser.run Game.gameInfoParser "GO[jo na]") (Result.Ok <| Game.info "GO" "jo na")
            ]
        ]
