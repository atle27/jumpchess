module JumpChess.GamePlay

open System
open JumpChess.MarbleLane
open JumpChess.MarbleJump
open JumpChess.GameBoard

type GamePlayer = {
    color:MarbleColor
    order:int // 0..5
    marbles:LaneCoord array }

type Game = {
    board:GameBoard
    players:GamePlayer list
    isSuperJump:bool }

type Move = LaneCoord list

let private isCircularStep (moves:Move) (step:LaneCoord) = 
    moves |> List.exists (fun moveStep -> step = (toAxisLaneCoord moveStep.axis moveStep))
    
let private isSingleStep (fromCoord:LaneCoord) (toCoord:LaneCoord) = 
    let fromAlignedCoord = toAxisLaneCoord toCoord.axis fromCoord
    toCoord.row = fromAlignedCoord.row && Math.Abs(toCoord.index - fromAlignedCoord.index) = 1

let rec private movesSpan game (move:Move) =
    let currentHole = move.Head 
    seq {
        for axis in 0..2 do
            let rotatedHole = toAxisLaneCoord axis currentHole
            let rotatedLane = game.board.[rotatedHole.axis].[gameBoardRow rotatedHole.row]
            let jumpIndices = jumpIndices rotatedLane rotatedHole.index game.isSuperJump
            if Seq.isEmpty jumpIndices 
            then yield move
            else 
                for i in jumpIndices do 
                    let nextMoveStep = { rotatedHole with index = i }
                    if isCircularStep move nextMoveStep then
                        yield move
                    elif isSingleStep currentHole nextMoveStep then
                        if move.Length = 1
                        then yield nextMoveStep::move
                        else yield move
                    else
                        let currentMove = nextMoveStep::move
                        yield! movesSpan game currentMove }
            
let allMoves game marbleHole = movesSpan game [marbleHole]
