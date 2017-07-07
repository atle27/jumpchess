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

let (=**) (m1:Move) (m2:Move) =
    m1.Head =* m2.Head && m1.Item(m1.Length-1) =* m2.Item(m2.Length-1)

let private isCircularStep (moves:Move) (step:LaneCoord) = 
    moves |> List.exists (fun moveStep -> (moveStep =* step))
    
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
            then yield! Seq.empty
            else 
                for i in jumpIndices do 
                    let nextMoveStep = { rotatedHole with index = i }
                    if isCircularStep move nextMoveStep then
                        yield! Seq.empty
                    elif isSingleStep currentHole nextMoveStep then
                        if move.Length = 1
                        then yield nextMoveStep::move
                        else yield! Seq.empty
                    else
                        let currentMove = nextMoveStep::move
                        yield nextMoveStep::move
                        yield! movesSpan game currentMove }
          

let distinctMoves (moves:seq<Move>) = 
    let mutable distinctMoveList = List.empty<Move>
    for move in moves do
        if not (distinctMoveList |> List.exists (fun distinctMove -> distinctMove =** move))
        then distinctMoveList <- move::distinctMoveList
    distinctMoveList
  
let allMoves game marbleHole = distinctMoves (movesSpan game [marbleHole])
