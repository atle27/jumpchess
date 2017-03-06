module JumpChess.GameBoard

open System
open JumpChess.Common
open JumpChess.MarbleLane
open JumpChess.MarbleJump

type BoardPosition = { 
    index:int; // ..-4, 0, 4.. i.e. use 4 as unit such that function 'toRotatedBoardLanePosition' only need integer addition and no fractions
    row:int; // ..-2, 0, 2.. i.e. use 2 as unit such that function 'toRotatedBoardLanePosition' only need integer addition and no fractions
    rotation:int }
    with 
    static member indexUnit = 4
    static member rowUnit = 2

type BoardLane = {
    lane:MarbleLane
    row:int
    rotation:int } 
    with 
    member x.length = 
        Array.length x.lane
    member x.axis = 
        Math.Abs(x.rotation % 3)
    member x.angle = 
        x.axis * 120
    member x.toMarbleLaneIndex boardLaneIndex =
        if isOdd x.length
        then boardLaneIndex + x.length-1/2
        else 
            if boardLaneIndex < 0
            then boardLaneIndex + x.length/2
            else boardLaneIndex + x.length/2 - 1
    member x.toBoardLaneIndex marbleLaneIndex =
        if isOdd x.length
        then marbleLaneIndex - (x.length-1)/2
        else 
            if marbleLaneIndex < x.length/2
            then marbleLaneIndex - (x.length)/2
            else marbleLaneIndex - (x.length)/2 + 1

let toRotatedBoardLanePosition (bp:BoardPosition) rotation =
        match (rotation % 3) with 
        | 1 -> { index = (bp.index / BoardPosition.indexUnit) * 2; 
                 row = (bp.row / BoardPosition.rowUnit) * -2; 
                 rotation = bp.rotation + 1 }
        | 2 -> { index = (bp.index / BoardPosition.indexUnit) * -4; 
                 row = bp.row; 
                 rotation = bp.rotation + 2 }
        | _ -> { index=bp.index; 
                 row=bp.row; 
                 rotation=bp.rotation}
  
type GameBoard = BoardLane array array // [axis,row]

let buildGameBoard () = 
    let buildMarbleLanes() =
        seq { 
            yield! buildLanes(seq {1..4})
            yield! buildLanes(seq {13..10})
            yield buildLane 9
            yield! buildLanes(seq {10..13})
            yield! buildLanes(seq {4..1}) 
        } |> Seq.toArray 
    let marbleLanes = 
        seq { for _ in 1 .. 3 -> buildMarbleLanes() } |> Seq.toArray
    let boardLane axis row = 
        { lane = marbleLanes.[axis].[row]; row = row; rotation = axis }
    let gameBoard =
        seq { for a in 1..3 -> seq { for r in 1..17 -> boardLane a r } |> Seq.toArray } |> Seq.toArray
    (gameBoard:GameBoard)



