open System
open JumpChess.Common
open JumpChess.MarbleLane
open JumpChess.GameBoard
open JumpChess.GamePlay
open JumpChess.GameStrategy
open JumpChess.GameRender

let savedGamePosition = """{ "board": [ [ [ "Black" ], [ "Black", "Black" ], [ "Black", "Black", "Empty" ], [ "Black", "Black", "Empty", "Black" ], [ "Empty", "Empty", "Empty", "Empty","Empty", "Empty", "Black", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty"], [ "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty" ], [ "Empty", "Empty", "Empty", "Empty", "Black", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty" ], [ "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty" ], [ "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty" ], [ "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty","Empty" ], [ "Empty", "Empty", "Empty", "Empty", "Blue", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty" ], [ "Empty", "Empty", "Empty", "Empty", "Empty","Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty" ], [ "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Blue", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty" ], [ "Blue", "Blue", "Empty", "Blue" ], [ "Blue", "Blue","Empty" ], [ "Blue", "Blue" ], [ "Blue" ] ], [ [ "Empty" ], [ "Empty", "Empty" ], [ "Empty", "Empty", "Empty" ], [ "Empty", "Empty", "Empty", "Empty" ], [ "Blue", "Blue", "Blue", "Blue", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty" ], [ "Blue", "Blue", "Blue", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty" ], [ "Empty", "Empty", "Blue", "Empty", "Blue", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty" ],[ "Blue", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty" ], [ "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Black", "Empty", "Empty" ], [ "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Black" ], [ "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Black", "Black", "Black" ], [ "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Black", "Black" ], [ "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Black", "Empty", "Black", "Black" ], [ "Empty", "Empty", "Empty", "Empty" ], [ "Empty", "Empty", "Empty" ], [ "Empty", "Empty" ], [ "Empty" ] ], [ ["Empty" ], [ "Empty", "Empty" ], [ "Empty", "Empty", "Empty" ], [ "Empty", "Empty", "Empty", "Empty" ], [ "Empty", "Empty", "Empty", "Empty", "Empty", "Empty","Empty", "Empty", "Empty", "Blue", "Empty", "Blue", "Blue" ], [ "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Blue", "Blue" ], [ "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Blue", "Blue", "Blue" ], [ "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Blue" ], [ "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Blue", "Empty", "Empty" ], [ "Black", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty" ], [ "Empty", "Empty", "Black", "Empty", "Black", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty" ], [ "Black", "Black", "Black", "Empty", "Empty", "Empty", "Empty", "Empty","Empty", "Empty", "Empty", "Empty" ], [ "Black", "Black", "Black", "Black", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty", "Empty" ], [ "Empty", "Empty", "Empty", "Empty" ], [ "Empty", "Empty", "Empty" ], [ "Empty", "Empty" ], [ "Empty" ] ] ], "isSuperJump": true, "players": [ { "color": "Black", "id": "1", "lastMove": null, "marbleCoords": [ [ 0, 8, 0 ], [ 0, 7, 0 ], [ 0, 7, 1 ], [ 0, 6, 0 ], [ 0, 6, 1 ], [ 2, -2, 4 ], [ 0, 5, 0 ], [ 0, 5, 1 ], [ 2, -2, 2 ], [ 0, 5, 3 ] ] }, { "color": "Blue", "id": "2", "lastMove": null, "marbleCoords": [ [ 0, -8, 0 ], [ 0, -7, 0 ], [ 0, -7, 1 ], [ 0, -6, 0 ], [ 0, -6, 1 ], [ 1, 2, 4 ], [ 0, -5, 0 ], [ 0, -5, 1 ], [ 1, 2, 2 ], [ 0, -5, 3 ] ] } ] }"""

let rec visualizeBoardPositions (marbleCoords:MarbleCoord list) gameBoard =
    match marbleCoords with
    | marbleCoord::tail -> visualizeBoardPositions tail (Board.add White marbleCoord gameBoard) 
    | [] -> gameBoard

let rec visualizeStrategyPlay (game:Game) (playerCount:int) (turn:int) = 
    let game = Game.doStrategyMove Strategy.bestMove game turn
    Render.consoleDraw <| game.board
    Console.WriteLine "Press a key for next turn..."
    Console.ReadKey() |> ignore
    visualizeStrategyPlay game playerCount ((turn+1)%playerCount)

[<EntryPoint>]
let main argv = 
    Console.SetWindowSize (55,37)
    
    let savedGame = Game.deserialize savedGamePosition

    Render.consoleDraw <| savedGame.board

    Console.WriteLine "Press key to see legal moves for marble..."

    Console.ReadKey() |> ignore

    let greenMarbleCoord = (0,2,4)

    let legalMovesGreenMarble = Strategy.legalMoves savedGame greenMarbleCoord |> Seq.toList

    Render.consoleDraw <| visualizeBoardPositions legalMovesGreenMarble savedGame.board 

    Console.WriteLine "Press a key to test fully loaded game board..."

    Console.ReadKey() |> ignore

    let isSuperJumpGame = true

    let players = ["A";"B";"C";"D";"E";"F"]

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
   
    let player = { id = "Atle"; color = Green; lastMove = None; marbleCoords= [| greenMarbleCoord |] }

    let game = { board = board; players = [ player ]; isSuperJump = isSuperJumpGame }

    let legalMovesGreenMarble = Strategy.legalMoves game greenMarbleCoord |> Seq.toList

    Render.consoleDraw <| visualizeBoardPositions legalMovesGreenMarble board 

    Console.WriteLine "Press a key to see the best move for green marble..."

    Console.ReadKey() |> ignore
   
    Render.consoleDraw <| (board |> (Strategy.bestMove >>* Board.move) game player)

    Console.WriteLine "Press a key to step through game between six AI players..."

    Console.ReadKey() |> ignore

    let allPlayerGame = Game.create true ["A";"B";"C";"D";"E";"F"]
    
    visualizeStrategyPlay allPlayerGame allPlayerGame.players.Length 0 |> ignore
      
    Console.WriteLine "Press a key to exit..."

    Console.ReadKey() |> ignore

    0
