open System
open JumpChess.Common
open JumpChess.MarbleLane
open JumpChess.GameBoard
open JumpChess.GamePlay
open JumpChess.GameStrategy

let renderMarbleHole (marbleHole:MarbleHole) =
    let marbleConsoleColor = 
        match marbleHole with
        | Marble(Black) -> Some(ConsoleColor.DarkMagenta)
        | Marble(White) -> Some(ConsoleColor.White)
        | Marble(Red) -> Some(ConsoleColor.Red)
        | Marble(Blue) -> Some(ConsoleColor.Blue)
        | Marble(Green) -> Some(ConsoleColor.Green)
        | Marble(Yellow) -> Some(ConsoleColor.Yellow)
        | Empty -> None
    Console.Write " "
    match marbleConsoleColor with
    | Some(color) -> 
        Console.BackgroundColor <- color
        Console.Write "  "
        Console.BackgroundColor <- ConsoleColor.Black
    | None ->
        Console.Write "()"
    Console.Write " "

let renderGameBoard (gameBoard:GameBoard) =
    Console.WriteLine " "
    for row in { 0 .. 16 } do
        let rowLength = gameBoard.[0].[row].Length
        let paddingCount = (13 - rowLength) / 2
        if isOdd rowLength 
        then Console.Write " "
        else Console.Write "   "
        for _ in { 1 .. paddingCount } do Console.Write "    "
        for index in { 0 .. rowLength-1 } do
            renderMarbleHole gameBoard.[0].[row].[index]
        Console.Write "\n \n"
        
[<EntryPoint>]
let main argv = 
    Console.SetWindowSize (55, 36)

    let marbleToMoveCoord = { index = 7; row = -1; rot = 0 } 

    let gameBoard = 
        Board.create 
        |> Board.add Red { index = 4; row = -2; rot = 1 } 
        |> Board.add Red { index = 7; row = -2; rot = 1 }
        |> Board.add Red { index = 5; row = 0; rot = 0 }
        |> Board.add Red { index = 6; row = -1; rot = 2 } 
        |> Board.add Red { index = 5; row = 4; rot = 0 } 
        |> Board.add Green marbleToMoveCoord 

    renderGameBoard gameBoard

    let game = { board = gameBoard; players = []; isSuperJump = true }

    Console.WriteLine "Press a key to see all legal moves for green marble..."

    Console.ReadKey() |> ignore

    let rec boardWithMoves (board:GameBoard) (moves:Move list) =
        match moves with
        | move::rest ->
            boardWithMoves (board |> Board.add White move.Head) rest
        | [] -> board

    renderGameBoard (boardWithMoves gameBoard (Seq.toList(Strategy.allMoves game marbleToMoveCoord)))

    Console.ReadKey() |> ignore

    0
