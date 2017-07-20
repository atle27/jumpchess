module JumpChess.GameStrategy

open JumpChess.GameBoard
open JumpChess.GamePlay

type Strategy() =
    static member legalMoves : Game -> MarbleCoord -> MarbleCoord seq = 
        fun game marblePosition -> 
            allLegalMoves game (laneCoord marblePosition) |> Seq.map (fun move -> marbleCoord move)
    static member bestMove : Game -> Player -> MarbleCoord = 
        fun game player -> 
            let allMoves =
                player.marbleCoords 
                |> Array.toSeq |> Seq.collect (fun marbleCoord -> Strategy.legalMoves game marbleCoord)
            let closestToGoal =
                allMoves
                |> Seq.map (fun marbleCoord ->
                    marbleCoord, (distance marbleCoord (goalCoord player.color)))
                |> Seq.reduce (fun (mc1,md1) (mc2, md2) ->
                    if md1 > md2
                    then (mc1,md1)
                    else (mc2,md2))
            let (bestMoveCoord,_) = closestToGoal
            bestMoveCoord
            
