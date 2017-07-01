module JumpChess.GameBoard

open System
open JumpChess.Common
open JumpChess.MarbleLane

// The game board has 3 axes with angles of 0 degrees, 120 degrees, and 240 degree.
// A board is represented as an array of marble lanes covering it along each axis.
// Hence each physical hole on the board belongs to 3 marble lanes, one along each axis.
// And hence a marble will always be located on 3 marble lanes, one along each axis.

type GameBoard = MarbleLane array array // [axis,row]

let buildGameBoard () = 
    let buildGameLanes () =
        seq { 
            yield! buildLanes(seq [1; 2; 3; 4])
            yield! buildLanes(seq [13; 12; 11; 10])
            yield buildLane 9
            yield! buildLanes(seq [10; 11; 12; 13])
            yield! buildLanes(seq [4; 3; 2; 1]) 
        } |> Seq.toArray 
    [| buildGameLanes(); buildGameLanes(); buildGameLanes(); |] : GameBoard

let gameBoard = buildGameBoard()

let gameLaneRowIndex boardLaneRow = 8 - boardLaneRow // board lane row is zero at the middle of board

[<CustomEquality; CustomComparison>]
type LaneCoord = { 
    // Coordinates for marble lanes on the game board
    index:int // lane index (value 0..(lanelength-1))
    row:int // lane row (value 0 at the center of the board)
    rot:int // lane rotation (in steps of 120 degrees, i.e. 2 = 240 degrees)
    } with
    member x.axis = Math.Abs(x.rot % 3)
    member x.angle = x.axis * 120
    member this.laneLength = gameBoard.[this.axis].[gameLaneRowIndex this.row].Length
    override x.Equals(obj) =
        match obj with
        | :? LaneCoord as y -> x.index = y.index && x.row = y.row && x.axis = y.axis
        | _ -> false
    override x.GetHashCode() = hash (x.index, x.row, x.axis)
    interface System.IComparable with
      member x.CompareTo obj =
          match obj with
          | :? LaneCoord as y -> compare (x.index, x.row, x.axis) (y.index, y.row, y.axis)
          | _ -> raise <| InvalidOperationException()

[<CustomEquality; CustomComparison>]
type BoardCoord = { 
    // Internal coordinate system.
    // Is used to convert between LaneCoord-s on lanes with different axes that corresponds to the same physical marble hole.
    // The BoardCoord uses 4 as unit on x-axis and 2 on y-axis such that the conversion function can be kept as integer calculations
    x:int; // lane index starting at 0 in the center of board and use 4 as unit, i.e. domain ..-8, -4, 0, 4, 8..
    y:int; // lane row starting at 0 in the center of board and use 2 as unit i.e. domain ..-4, -2, 0, 2, 4..
    rot:int // lane rotation in steps of 120 degrees, i.e. 2 = 240 degrees
    } with 
    member x.axis = Math.Abs(x.rot % 3)
    member this.laneLength = gameBoard.[this.axis].[gameLaneRowIndex (this.y / BoardCoord.yUnit)].Length
    static member xUnit = 4
    static member yUnit = 2
      override x.Equals(obj) =
        match obj with
        | :? BoardCoord as y -> x.x = y.x && x.y = y.y && x.axis = y.axis
        | _ -> false
    override x.GetHashCode() = hash (x.x, x.y, x.rot)
    interface System.IComparable with
      member x.CompareTo obj =
          match obj with
          | :? BoardCoord as y -> compare (x.x, x.y, x.axis) (y.x, y.y, y.axis)
          | _ -> raise <| InvalidOperationException()

let toLaneCoord (coord:BoardCoord) =
    let centeredIndex = coord.x / BoardCoord.xUnit
    let index = 
        if isOdd coord.laneLength
        then centeredIndex + (coord.laneLength-1)/2
        else 
            if centeredIndex < 0
            then centeredIndex + coord.laneLength/2
            else centeredIndex + coord.laneLength/2 - 1
    { index = index; row = coord.y / BoardCoord.yUnit; rot = coord.rot }

let toBoardCoord (coord:LaneCoord) =
    let centeredIndex = 
        if isOdd coord.laneLength
        then coord.index - (coord.laneLength-1)/2
        else 
            if coord.index < coord.laneLength/2
            then coord.index - (coord.laneLength)/2
            else coord.index - (coord.laneLength)/2 + 1
    { x = centeredIndex * BoardCoord.xUnit; y = coord.row * BoardCoord.yUnit; rot = coord.axis }

let private toRotatedCoord rotation (coord:BoardCoord) =
        match (rotation % 3) with 
        | 1 -> { x = (coord.x / BoardCoord.xUnit) * 2; 
                 y = (coord.y / BoardCoord.yUnit) * -2; 
                 rot = coord.rot + 1 }
        | 2 -> { x = (coord.x / BoardCoord.xUnit) * -4; 
                 y = coord.y; 
                 rot = coord.rot + 2 }
        | _ -> coord

let rotatedLaneCoord rotation (coord:LaneCoord) =
    (toBoardCoord >> toRotatedCoord rotation >> toLaneCoord) coord


