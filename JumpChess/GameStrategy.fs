module JumpChess.GameStrategy

open JumpChess.GameBoard
open JumpChess.GamePlay

type Strategy() =
    static member legalMoves : Game -> MarbleCoord -> MarbleCoord seq = 
        fun game marblePosition -> 
            allLegalMoves game (laneCoord marblePosition) |> Seq.map (fun move -> marbleCoord move)
    static member bestMove : Game -> MarbleCoord -> MarbleCoord = 
        // to be changed to simple strategy of longest distance move...
        fun game marble -> Seq.toArray(Strategy.legalMoves game marble).[0]
