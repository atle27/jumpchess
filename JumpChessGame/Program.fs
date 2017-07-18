open System
open JumpChess.Common
open JumpChess.MarbleLane
open JumpChess.GameBoard
open JumpChess.GamePlay
open JumpChess.GameStrategy
open JumpChess.GameRender

[<EntryPoint>]
let main argv = 
    Console.SetWindowSize (55, 36)

    let greenMarbleCoord = { index = 7; row = -1; rot = 0 } 

    let gameBoard = 
        Board.create 
        |> Board.add Red { index = 4; row = -2; rot = 1 } 
        |> Board.add Red { index = 7; row = -2; rot = 1 }
        |> Board.add Red { index = 5; row = 0; rot = 0 }
        |> Board.add Red { index = 6; row = -1; rot = 2 } 
        |> Board.add Red { index = 5; row = 4; rot = 0 } 
        |> Board.add Green greenMarbleCoord 

    Render.drawConsole <| gameBoard

    Console.WriteLine "Press a key to see all legal moves for green marble..."

    Console.ReadKey() |> ignore
   
    let game = { board = gameBoard; players = []; isSuperJump = true }

    let allMovesGreenMarble = Strategy.allMoves game greenMarbleCoord |> Seq.toList

    let rec addMarblesForMovesToBoard (board:GameBoard) (moves:Move list) =
        match moves with
        | move::rest ->
            addMarblesForMovesToBoard (board |> Board.add White move.Head) rest
        | [] -> board

    Render.drawConsole <| (addMarblesForMovesToBoard gameBoard allMovesGreenMarble)

    Console.ReadKey() |> ignore

    0
