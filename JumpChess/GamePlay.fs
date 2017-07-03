module JumpChess.GamePlay

open JumpChess.MarbleLane
open JumpChess.MarbleJump
open JumpChess.GameBoard

type GamePlayer = {
    color:MarbleColor
    order:int // 0..5
    marbles:LaneCoord Set }

type Game = {
    board:GameBoard
    players:GamePlayer list
    isSuperJump:bool }

type Move = LaneCoord list

let rec private movesSpan game (move:Move) =
    let currentHole = move.Head 
    seq {
        for rotation in 0..2 do
            let rotatedHole = toRotatedLaneCoord rotation currentHole
            let rotatedLane = game.board.[rotatedHole.axis].[gameLaneRowIndex rotatedHole.row]
            let jumpIndices = jumpIndices rotatedLane rotatedHole.index game.isSuperJump
            if Seq.isEmpty jumpIndices 
            then yield move
            else for i in jumpIndices do 
                    let currentMove = { rotatedHole with index = i }::move
                    yield! movesSpan game currentMove }
            
let allMoves game marbleHole = movesSpan game [marbleHole]
