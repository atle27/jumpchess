open System
open JumpChess.Common
open JumpChess.MarbleLane
open JumpChess.GameBoard

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
    renderGameBoard gameBoard
    Console.ReadKey() |> ignore
    0
