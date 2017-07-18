open System
open JumpChess.Common
open JumpChess.MarbleLane
open JumpChess.GameBoard
open JumpChess.GamePlay
open JumpChess.GameStrategy
open JumpChess.GameRender

[<EntryPoint>]
let main argv = 
    Console.SetWindowSize (55,36)

    let greenMarbleCoord = (0,-1,7)

    let gameBoard = 
        Board.create 
        |> Board.add Red (1,-2,4)
        |> Board.add Red (1,-2,7)
        |> Board.add Red (0,0,5)
        |> Board.add Red (2,-1,6) 
        |> Board.add Red (0,4,5)
        |> Board.add Green greenMarbleCoord 

    Render.consoleDraw <| gameBoard

    Console.WriteLine "Press a key to see all legal moves for green marble..."

    Console.ReadKey() |> ignore
   
    let game = { board = gameBoard; players = []; isSuperJump = true }

    let legalMovesGreenMarble = Strategy.legalMoves game greenMarbleCoord |> Seq.toList

    let rec addMovesToBoard (marbleCoords:MarbleCoord list) gameBoard =
        match marbleCoords with
        | marbleCoord::tail -> addMovesToBoard tail (Board.add White marbleCoord gameBoard) 
        | [] -> gameBoard

    Render.consoleDraw <| addMovesToBoard legalMovesGreenMarble gameBoard 

    Console.ReadKey() |> ignore

    0
