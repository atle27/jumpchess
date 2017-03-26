module JumpChess.GameBoard

open System
open JumpChess.Common
open JumpChess.MarbleLane

// The game board has 3 axes with angles of 0 degrees, 120 degrees, and 240 degree.
// The board has 3 parallel coordinate systems, one for each board axes.
// Each physical hole on the board has 3 possible coordinates, which one to use depends on the axis/angle of the marble lane being referenced.

type GameBoard = MarbleLane array array // [axis,row]

let buildGameBoard () = 
    let buildLanes() =
        seq { 
            yield! buildLanes(seq {1..4})
            yield! buildLanes(seq {13..10})
            yield buildLane 9
            yield! buildLanes(seq {10..13})
            yield! buildLanes(seq {4..1}) 
        } |> Seq.toArray 
    [| buildLanes(); buildLanes(); buildLanes(); |] : GameBoard

let gameBoard = buildGameBoard()

type HoleCoord = { 
    // Hole coordinates for marble lanes on the game board
    index:int // lane index (value 0..(lanelength-1))
    row:int // lane row (value 0 at the center of the board)
    rot:int // lane rotation (in steps of 120 degrees, i.e. 2 = 240 degrees)
    } with
    member x.axis = Math.Abs(x.rot % 3)
    member x.angle = x.axis * 120
    member this.laneLength = gameBoard.[this.rot].[this.row].Length

type private BoardCoord = { 
    // Internal game board coordinate system.
    // Is used to convert between HoleCoord-s on lanes with different axes that corresponds to the same physical marble hole.
    // The BoardCoord uses 4 as unit on x-axis and 2 on y-axis such that the conversion function can be kept as integer calulations
    x:int; // lane index starting at 0 in the center of board and use 4 as unit, i.e. domain ..-8, -4, 0, 4, 8..
    y:int; // lane row starting at 0 in the center of board and use 2 as unit i.e. domain ..-4, -2, 0, 2, 4..
    rot:int // lane rotation in steps of 120 degrees, i.e. 2 = 240 degrees
    } with 
    member this.laneLength = gameBoard.[this.rot].[this.y].Length
    static member xUnit = 4
    static member yUnit = 2

let private toHoleCoord (coord:BoardCoord) =
    let index = 
        if isOdd coord.laneLength
        then coord.x + coord.laneLength - 1/2
        else 
            if coord.x < 0
            then coord.x + coord.laneLength/2
            else coord.x + coord.laneLength/2 - 1
    { index = index; row = coord.y; rot = coord.rot }

let private toBoardCoord (coord:HoleCoord) =
    let x = 
        if isOdd coord.laneLength
        then coord.index - (coord.laneLength-1)/2
        else 
            if coord.index < coord.laneLength/2
            then coord.index - (coord.laneLength)/2
            else coord.index - (coord.laneLength)/2 + 1
    { x = x; y = coord.row; rot = coord.axis }

let private toRotatedCoord rotation (coord:BoardCoord) =
        match (rotation % 3) with 
        | 1 -> { x = (coord.x / BoardCoord.xUnit) * 2; 
                 y = (coord.y / BoardCoord.yUnit) * -2; 
                 rot = coord.rot + 1 }
        | 2 -> { x = (coord.x / BoardCoord.xUnit) * -4; 
                 y = coord.y; 
                 rot = coord.rot + 2 }
        | _ -> coord

let rotatedLaneCoord rotation (coord:HoleCoord) =
    (toBoardCoord >> toRotatedCoord rotation >> toHoleCoord) coord


