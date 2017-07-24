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

    let board = 
        Board.create () 
        |> Board.add Red (1,-2,4)
        |> Board.add Red (1,-2,7)
        |> Board.add Red (0,0,5)
        |> Board.add Red (2,-1,6) 
        |> Board.add Red (0,4,5)
        |> Board.add Green greenMarbleCoord 

    Render.consoleDraw <| board

    Console.WriteLine "Press a key to see all legal moves for green marble..."

    Console.ReadKey() |> ignore
   
    let player = { id = "Atle"; color = Green; marbleCoords= [| greenMarbleCoord |] }

    let game = { board = board; players = [ player ]; isSuperJump = isSuperJumpGame }

    let legalMovesGreenMarble = Strategy.legalMoves game greenMarbleCoord |> Seq.toList

    let rec addMovesToBoard (marbleCoords:MarbleCoord list) gameBoard =
        match marbleCoords with
        | marbleCoord::tail -> addMovesToBoard tail (Board.add White marbleCoord gameBoard) 
        | [] -> gameBoard

    Render.consoleDraw <| addMovesToBoard legalMovesGreenMarble board 

    Console.WriteLine "Press a key to see the best move for green marble..."

    Console.ReadKey() |> ignore
   
    Render.consoleDraw <| (board |> (Strategy.bestMove >>* Board.move) game player)

    Console.WriteLine "Press a key exit..."

    Console.ReadKey() |> ignore

    0
