module JumpChess.GameStrategy

open JumpChess.MarbleLane
open JumpChess.GameBoard
open JumpChess.GamePlay

type Strategy() =
    static member legalMoves : Game -> MarbleCoord -> MarbleCoord seq = 
        fun game marblePosition -> 
            allLegalMoves game (laneCoord marblePosition) |> Seq.map (fun move -> marbleCoord move)

    static member validMoves : Game -> Player -> MarbleCoord -> MarbleCoord seq = 
        // valid moves are the subset of legal moves that is on the main board 
        // or in the start area of marble or the goal area of marble (which is the 
        // start area of opposite colored marbles), e.g. it is not a valid move to 
        // jump into the goal or start area or the ajacent marble color.
        fun game player marblePosition -> 
            let allMoves = Strategy.legalMoves game marblePosition
            let validColors = [| player.color; oppositeColor player.color |]
            let invalidCoords = 
                marbleColors
                |> Seq.filter (fun c -> c <> validColors.[0] && c <> validColors.[0])
                |> Seq.collect (fun c -> (startCoords c |> Array.toSeq)) : MarbleCoord seq
            let isInvalidCoord coord =
                invalidCoords |> Seq.exists (fun c -> c = coord)
            allMoves |> Seq.filter (fun coord -> not(isInvalidCoord coord))

    static member bestMove : StrategyBestMove = 
        fun game player -> 
            let allJumps = // a sequence of the valid moves for the players marbles
                player.marbleCoords 
                |> Array.toSeq 
                |> Seq.map (fun mc -> mc, Strategy.validMoves game player mc) 

            let theBestMove = // best move for the player
                allJumps   
                |> Seq.collect (fun (startCoord, endCoords) -> // flatten to all valid moves
                    endCoords |> Seq.map (fun endCoord -> startCoord, endCoord))
                |> Seq.filter (fun (startCoord, endCoord) -> // filter out stale (repeat) moves
                    Some(endCoord, startCoord) <> player.lastMove)
                |> Seq.map (fun (startCoord, endCoord) ->
                    startCoord, // marble location before mpve
                    endCoord, // marble location after move
                    (distance startCoord (goalCoord player.color)), // distance to goal before move
                    (distance endCoord (goalCoord player.color)), // distance to goal after move
                    (distance startCoord (goalCoord (oppositeColor player.color)))) // distance from start before move
                |> Seq.reduce (fun (m1sc,m1ec,m1d1,m1d2,m1d3) (m2sc,m2ec,m2d1,m2d2,m2d3) -> 
                    // REDUCE VALID MOVES TO THE BEST MOVE
                    if (m1d1 < 2.5 && m2d1 > 2.5) // prioritize marble that has not reached goal area
                    then (m2sc, m2ec, m2d1, m2d2, m2d3)
                    elif (m2d1 < 2.5 && m1d1 > 2.5) // prioritize marble that has not reached goal area
                    then (m1sc, m1ec, m1d1, m1d2, m1d3)
                    else
                        let m1valid = m1d2 <= m1d1 // to move away from goal is not valid
                        let m2valid = m2d2 <= m2d1 // to move away from goal is not valid
                        if m1valid && m2valid
                        then
                            if (m1d3 < 2.5 && m2d3 > 2.5) // prioritize marble in start area
                            then (m1sc, m1ec, m1d1, m1d2, m1d3) 
                            elif (m2d3 < 2.5 && m1d3 > 2.5) // prioritize marble in start area
                            then (m2sc, m2ec, m2d1, m2d2, m2d3) 
                            elif m1d2 < m2d2 // else prioritize longest jump
                            then (m1sc, m1ec, m1d1, m1d2, m1d3)
                            else (m2sc, m2ec, m2d1, m2d2, m2d3)
                        elif m1valid 
                        then
                            (m1sc, m1ec, m1d1, m1d2, m1d3)
                        else
                            (m2sc, m2ec, m2d1, m2d2, m2d3))

            let (startCoord, endCoord, _, _, _) = theBestMove
            
            startCoord,endCoord // start and end coord of the best move
            
