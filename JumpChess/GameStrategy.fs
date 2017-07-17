module JumpChess.GameStrategy

open JumpChess.GameBoard
open JumpChess.GamePlay

type Strategy() =
    static member allMoves : (Game -> LaneCoord -> Move seq) = 
        fun game marble -> allMoves game marble
    static member bestMove : (Game -> LaneCoord -> Move) = // to be changes to simple strategy of longest distance move 
        fun game marble -> Seq.toArray(Strategy.allMoves game marble).[0]
