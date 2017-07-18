open System
open JumpChess.Common
open JumpChess.MarbleLane
open JumpChess.GameBoard
open JumpChess.GamePlay
open JumpChess.GameStrategy
open JumpChess.GameRender

[<EntryPoint>]
let main argv = 
    Console.SetWindowSize (55,37)

    let isSuperJumpGame = true

    let players = ["A",Blue;"B",Red;"C",Green;"D",White;"E",Black;"F",Yellow]

    let newGame = Game.create isSuperJumpGame players

    Render.consoleDraw <| newGame.board

    Console.WriteLine "Press a key to see example of legal moves..."

    Console.ReadKey() |> ignore

    let greenMarbleCoord = (0,-1,7)

    let testBoard = 
        Board.create () 
        |> Board.add Red (1,-2,4)
        |> Board.add Red (1,-2,7)
        |> Board.add Red (0,0,5)
        |> Board.add Red (2,-1,6) 
        |> Board.add Red (0,4,5)
        |> Board.add Green greenMarbleCoord 

    Render.consoleDraw <| testBoard

    Console.WriteLine "Press a key to see all legal moves for green marble..."

    Console.ReadKey() |> ignore
   
    let testGame = { board = testBoard; players = []; isSuperJump = isSuperJumpGame }

    let legalMovesGreenMarble = Strategy.legalMoves testGame greenMarbleCoord |> Seq.toList

    let rec addMovesToBoard (marbleCoords:MarbleCoord list) gameBoard =
        match marbleCoords with
        | marbleCoord::tail -> addMovesToBoard tail (Board.add White marbleCoord gameBoard) 
        | [] -> gameBoard

    Render.consoleDraw <| addMovesToBoard legalMovesGreenMarble testBoard 

    Console.WriteLine "Press a key exit..."

    Console.ReadKey() |> ignore

    0
