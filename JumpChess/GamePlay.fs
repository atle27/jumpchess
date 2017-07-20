module JumpChess.GamePlay

open System
open JumpChess.Common
open JumpChess.MarbleLane
open JumpChess.MarbleJump
open JumpChess.GameBoard

type Player = {
    id:string
    color:MarbleColor
    marbleCoords:MarbleCoord array }

let playOrder marbleColor = 
    match marbleColor with
    | Black -> 1 | White -> 2 | Red -> 3 | Blue -> 4 | Green -> 5 | Yellow -> 6

let goalCoord marbleColor =
    let order = (playOrder marbleColor) - 1
    let sign = 1 - 2 * (order % 2)
    let axis = order % 3
    (axis,-8*sign,0):MarbleCoord   

type Game = {
    board:GameBoard
    players:Player list
    isSuperJump:bool } with
    static member create : bool -> (string*MarbleColor) list -> Game = 
        fun isSuperJump gamePlayers -> 
            let players = 
                gamePlayers 
                |> List.map (fun (id,color) -> 
                    let order = (playOrder color) - 1
                    let sign = 1 - 2 * (order % 2)
                    let axis = order % 3
                    let marbles = [| 
                        for row in {8*sign..(-1*sign)..5*sign} do 
                            for index in {0..8-row*sign} -> 
                                axis,row,index |]
                    { id=id; color=color; marbleCoords=marbles })
            let marbles = 
                players 
                |> List.collect (fun player -> 
                    player.marbleCoords 
                    |> Array.toList 
                    |> List.map (fun marble -> player.color,marble))
            let rec loadBoard marbles board =
                match marbles with
                | (marbleColor,marbleCoord)::rest -> 
                    loadBoard rest (board |> Board.add marbleColor marbleCoord)
                | [] -> board
            let board = Board.create() |> loadBoard marbles
            { board = board; players=players; isSuperJump=isSuperJump }

type internal Jump = LaneCoord list

let internal (=**) (j1:Jump) (j2:Jump) =
    let jumpHeadMatches = j1.Head =* j2.Head
    let jumpTailMatches = j1.Item(j1.Length-1) =* j2.Item(j2.Length-1)
    jumpHeadMatches && jumpTailMatches

let private isCircularStep (jump:Jump) (step:LaneCoord) = 
    jump |> List.exists (fun jumpStep -> (jumpStep =* step))
    
let private isSingleStep (fromCoord:LaneCoord) (toCoord:LaneCoord) = 
    let fromAlignedCoord = toAxisLaneCoord toCoord.axis fromCoord
    toCoord.row = fromAlignedCoord.row && Math.Abs(toCoord.index - fromAlignedCoord.index) = 1

let rec private jumpsSpan (game:Game) (jump:Jump) =
    let currentHole = jump.Head 
    seq {
        for axis in 0..2 do
            let rotatedHole = toAxisLaneCoord axis currentHole
            let rotatedLane = game.board.[rotatedHole.axis].[gameBoardRow rotatedHole.row]
            let jumpIndices = jumpIndices rotatedLane rotatedHole.index game.isSuperJump
            if Seq.isEmpty jumpIndices 
            then yield! Seq.empty
            else 
                for i in jumpIndices do 
                    let nextJumpStep = { rotatedHole with index = i }
                    if isCircularStep jump nextJumpStep then
                        yield! Seq.empty
                    elif isSingleStep currentHole nextJumpStep then
                        if jump.Length = 1
                        then yield nextJumpStep::jump
                        else yield! Seq.empty
                    else
                        let currentJump = nextJumpStep::jump
                        yield nextJumpStep::jump
                        yield! jumpsSpan game currentJump }
          
let private distinctJumps (jumps:seq<Jump>) = 
    let jumpArray = jumps |> Seq.toArray
    let jumpCount = jumpArray.Length
    seq {
        for jumpIndex in { 0 .. jumpCount-1 } do
            let lastEntry = Array.FindIndex(
                                jumpArray, 
                                jumpIndex+1, 
                                (fun jump -> jump =** jumpArray.[jumpIndex])) = -1
            if (lastEntry)
            then yield jumpArray.[jumpIndex]
            else yield! Seq.empty }
   
let internal allLegalMoves game marblePosition = 
    distinctJumps (jumpsSpan game [marblePosition]) |> Seq.map (fun jump -> jump.Head)
