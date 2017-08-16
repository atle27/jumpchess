module JumpChess.GamePlay

open System
open JumpChess.Common
open JumpChess.MarbleLane
open JumpChess.MarbleJump
open JumpChess.GameBoard
open Chiron
open Chiron.Operators

type Player = {
    id:string
    color:MarbleColor
    lastMove:((MarbleCoord*MarbleCoord) option)
    marbleCoords:MarbleCoord array } with 
    static member ToJson (x:Player) =
           Json.write "id" x.id
        *> Json.write "color" x.color.toString
        *> Json.write "lastMove" x.lastMove
        *> Json.write "marbleCoords" x.marbleCoords
    static member FromJson (_:Player) =
        json {
            let! id = Json.read "id"
            let! color = Json.read "color" 
            let! lastMove = Json.read "lastMove" 
            let! marbleCoords = Json.read "marbleCoords"
            return { id = id; color = MarbleColor.fromString(color); lastMove = lastMove; marbleCoords = marbleCoords } }

let playOrder marbleColor = 
    1 + Array.IndexOf(marbleColors,marbleColor)

let oppositeColor marbleColor = 
    marbleColors.[(((playOrder marbleColor) - 1) + 3) % 6]

let goalCoord marbleColor =
    let order = (playOrder marbleColor) - 1
    let sign = 1 - 2 * (order % 2)
    let axis = order % 3
    (axis,-8*sign,0):MarbleCoord   

let startCoords marbleColor =
    let order = (playOrder marbleColor) - 1
    let sign = 1 - 2 * (order % 2)
    let axis = order % 3
    [| for row in {8*sign..(-1*sign)..5*sign} do 
        for index in {0..8-row*sign} -> 
            axis,row,index |]


type Game = {
    board:GameBoard
    players:Player list
    isSuperJump:bool } with
    static member ToJson (x:Game) =
           Json.write "board" x.board
        *> Json.write "players" x.players
        *> Json.write "isSuperJump" x.isSuperJump
    static member FromJson (_:Game) =
        json {
            let! board = Json.read "board"
            let! players = Json.read "players" 
            let! isSuperJump = Json.read "isSuperJump"
            return { board = board; players = players; isSuperJump = isSuperJump } }
    static member serialize : Game -> string =
        fun game ->
            game
            |> Json.serialize
            |> Json.formatWith JsonFormattingOptions.SingleLine
    static member deserialize : string -> Game =
        fun string ->
            string
            |> Json.parse
            |> Json.deserialize
    static member create : bool -> string list -> Game = 
        fun isSuperJump playersIds -> 
            let colors = 
                match  playersIds.Length with
                | 2 -> marbleColors.[0]::marbleColors.[3]::[]
                | 4 -> marbleColors.[0]::marbleColors.[1]::marbleColors.[3]::marbleColors.[4]::[]
                | 6 -> Array.toList marbleColors
                | _ -> failwith "Invalid number of players"
            let gamePlayers = List.zip playersIds colors
            let players = 
                gamePlayers 
                |> List.map (fun (id,color) -> 
                    { id=id; color=color; lastMove = None; marbleCoords=(startCoords color) })
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
    static member doStrategyMove : StrategyMove -> Game -> int -> Game =
        fun (strategyMove:StrategyMove) (game:Game) (turn:int) ->
            let mutable player = game.players.[turn]
            let doPlayerBoardMove moveFrom moveTo =
                player <- { 
                    id = player.id; 
                    color = player.color; 
                    lastMove = Some(moveFrom, moveTo);
                    marbleCoords = player.marbleCoords |> Array.map (fun c -> if c = moveFrom then moveTo else c) }
                Board.move moveFrom moveTo
            let doBoardMove = (strategyMove >>* doPlayerBoardMove) game player
            { board = game.board |> doBoardMove
              players = game.players |> List.map (fun p -> if p.id = player.id then player else p)
              isSuperJump = game.isSuperJump }
and StrategyMove = Game -> Player -> MarbleCoord * MarbleCoord

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
          
let private notSelfJumps selfPosition (jumps:seq<Jump>) =
    let isJumpOver fromCoord toCoord middleCoord =
        match commonAxis fromCoord toCoord with
        | None -> failwith "Logic error, jump with no jump axis!"
        | Some(axis) ->
            let sc = toAxisLaneCoord axis fromCoord
            let ec = toAxisLaneCoord axis toCoord
            if (Math.Abs(sc.index - ec.index) < 2)
            then false
            else
                let jumpOverCoord = { 
                        index = (sc.index + ec.index) / 2
                        row = sc.row 
                        rot = sc.rot } 
                jumpOverCoord =* middleCoord
    seq { for jump in jumps do
            let ja = jump |> List.toArray
            let jumpPairs = Array.zip (ja.[0..ja.Length-2]) (ja.[1..ja.Length-1])
            let isJumpOver = Array.Exists(jumpPairs, (fun (a,b) -> isJumpOver a b selfPosition ))
            if isJumpOver
            then yield! Seq.empty
            else yield jump }

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
    notSelfJumps marblePosition (distinctJumps (jumpsSpan game [marblePosition])) 
    |> Seq.map (fun jump -> jump.Head)
