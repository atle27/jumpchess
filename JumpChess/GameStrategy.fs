module JumpChess.GameStrategy

open JumpChess.MarbleLane
open JumpChess.GameBoard
open JumpChess.GamePlay

type Strategy() =
    static member legalMoves : Game -> MarbleCoord -> MarbleCoord seq = 
        fun game marblePosition -> 
            allLegalMoves game (laneCoord marblePosition) |> Seq.map (fun move -> marbleCoord move)
    static member validMoves : Game -> Player -> MarbleCoord -> MarbleCoord seq= 
        fun game player marblePosition -> 
            let allMoves = Strategy.legalMoves game marblePosition
            let validColors = [| player.color; oppositeColor player.color |]
            let invalidCoords = 
                marbleColors
                |> Seq.filter (fun color -> color <> validColors.[0] && color <> validColors.[0])
                |> Seq.collect (fun color -> (startCoords color |> Array.toSeq)) : MarbleCoord seq
            allMoves |> Seq.filter (fun move -> not (invalidCoords |> Seq.exists (fun invalidCoord -> invalidCoord = move)))
    static member bestMove : Game -> Player -> MarbleCoord * MarbleCoord = 
        fun game player -> 
            let allJumps =
                player.marbleCoords 
                |> Array.toSeq 
                |> Seq.map (fun marbleCoord -> marbleCoord, Strategy.validMoves game player marbleCoord)
            let closestToGoal =
                allJumps
                |> Seq.collect (fun (startCoord, endCoords) ->
                    endCoords |> Seq.map (fun endCoord -> startCoord, endCoord))
                |> Seq.map (fun (startCoord, endCoord) ->
                    startCoord, endCoord, (distance endCoord (goalCoord player.color)))
                |> Seq.reduce (fun (msc1,mec1,md1) (msc2, mec2, md2) ->
                    if md1 < md2 
                    then (msc1, mec1, md1)
                    else (msc2, mec2, md2))
            let (startCoord,endCoord,_) = closestToGoal
            startCoord,endCoord
            
