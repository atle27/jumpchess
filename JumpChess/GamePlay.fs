module JumpChess.GamePlay

open JumpChess.MarbleLane
open JumpChess.MarbleJump
open JumpChess.GameBoard

type GamePlayer = {
    color:MarbleColor
    order:int // 0..5
    marbles:(HoleCoord Set) }

type Game = {
    board:GameBoard
    players:GamePlayer list
    isSuperJump:bool }

type Move = HoleCoord list

let rec private movesSpan (moves:Move) (game:Game) =
    let hole = moves.Head 
    let lane = game.board.[hole.axis].[hole.row]
    seq {
        for rotation in 0..2 do
            let rotatedHole = rotatedLaneCoord lane.Length rotation hole
            let rotatedLane = game.board.[rotatedHole.axis].[rotatedHole.row]
            let jumpIndices = jumpIndices rotatedLane rotatedHole.index game.isSuperJump
            if Seq.isEmpty jumpIndices 
            then yield moves
            else for index in jumpIndices do
                    let nextHole = { rotatedHole with index = index }
                    let newMove = nextHole :: moves
                    yield newMove }
            
let allMoves (hole:HoleCoord) = movesSpan [hole]
