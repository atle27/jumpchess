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
    [| for axis in {0..2} -> 
        seq { 
            yield! buildLanes({1..1..4})
            yield! buildLanes({13..-1..10})
            yield buildLane 9
            yield! buildLanes({10..1..13})
            yield! buildLanes({4..-1..1}) 
        } |> Seq.toArray |] : GameBoard

let emptyGameBoard = buildGameBoard()

let gameBoardRow row = 8 - row // converts from rows that are zero at the middle of board to game board rows

let gameBoardAxis rotation = ((rotation % 3) + 3) % 3 // converts from a rotation to one of the 3 major game board axes.

[<CustomEquality; CustomComparison>]
type LaneCoord = { 
    // Coordinate system for the marble lanes on the game board
    // Is used for game play.
    index:int // lane index (value 0..(lanelength-1))
    row:int // lane row (value 0 at the center of the board)
    rot:int // lane rotation (in steps of 120 degrees, i.e. 2 = 240 degrees)
    } with
    member x.axis = gameBoardAxis x.rot
    member x.angle = x.axis * 120
    member this.laneLength = emptyGameBoard.[this.axis].[gameBoardRow this.row].Length
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
    member this.laneLength = emptyGameBoard.[this.axis].[gameBoardRow (this.y / BoardCoord.yUnit)].Length
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

let private halfUnitAdjustment (x:int) isOdddLaneLength = 
    // Marble lanes with even length is shifted with 1/2 a coordinate-unit in the x-direction. 
    // This is done because the marbles are not lying in a rectangular grid but the marble hole 
    // of a ajacent marble lane are shifted 1/2 unit.
    if isOdddLaneLength then 0 else Math.Sign(x) * BoardCoord.xUnit/2

let toLaneCoord (coord:BoardCoord) =
    let isOdddLaneLength = isOdd coord.laneLength
    let index = (coord.x + (halfUnitAdjustment coord.x isOdddLaneLength)) / BoardCoord.xUnit
    let centeredIndex = 
        if isOdddLaneLength
        then index + (coord.laneLength-1)/2
        else 
            if index < 0
            then index + coord.laneLength/2
            else index + coord.laneLength/2 - 1
    { index = centeredIndex; row = coord.y / BoardCoord.yUnit; rot = coord.rot }

let toBoardCoord (coord:LaneCoord) =
    let isOdddLaneLength = isOdd coord.laneLength
    let centeredIndex = 
        if isOdddLaneLength
        then coord.index - (coord.laneLength-1)/2
        else 
            if coord.index < coord.laneLength/2
            then coord.index - (coord.laneLength)/2
            else coord.index - (coord.laneLength)/2 + 1
    { x = centeredIndex * BoardCoord.xUnit - (halfUnitAdjustment centeredIndex isOdddLaneLength); 
      y = coord.row * BoardCoord.yUnit; rot = coord.axis }

let private toRotatedCoord rotation (coord:BoardCoord) =
    let rot = gameBoardAxis rotation
    let xCoordTransform = 
        if rot = 1 then
            if (coord.x <> 0) 
            then -coord.x / 2, -coord.x / 2
            else 0, 0
        elif rot = 2 then
            if (coord.x <> 0) 
            then -coord.x / 2, coord.x / 2
            else 0, 0
        else
            coord.x, 0
    let yCoordTransform = 
        if rot = 1 then
            if (coord.y <> 0) 
            then 3 * (coord.y / 2), -coord.y / 2
            else 0, 0
        elif rot = 2 then
            if (coord.y <> 0) 
            then -3 * (coord.y / 2), -coord.y / 2
            else 0, 0
        else
            0, coord.y
    let x1,y1 = xCoordTransform
    let x2,y2 = yCoordTransform
    { x = x1+x2; y = y1+y2; rot = coord.rot+rot }

let toRotatedLaneCoord rotation (coord:LaneCoord) =
    (toBoardCoord >> toRotatedCoord rotation >> toLaneCoord) coord

let toAxisLaneCoord axis (coord:LaneCoord) =
    toRotatedLaneCoord (axis - coord.axis) coord

let reduceGameBoardAxisLaneState (gameBoard:GameBoard) (axis:int, row:int, newLaneState:MarbleLane) =     
    [| for a in { 0 .. 2 } ->
         [| for r in { 0 .. gameBoard.[a].Length-1 } ->
                if (a=axis && r=row)
                then newLaneState
                else gameBoard.[a].[r] |] |] : GameBoard

let rec reduceGameBoardAllAxisStates (gameBoard:GameBoard) (marbleHolePosition:LaneCoord, newMarbleHoleState) (startAxis:int) = 
    let i = marbleHolePosition.index
    let r = gameBoardRow marbleHolePosition.row
    let a = marbleHolePosition.axis
    let newLane = reduceMarbleLaneState gameBoard.[a].[r] (i,newMarbleHoleState)
    let newGameBoard = reduceGameBoardAxisLaneState gameBoard (a, r, newLane)
    let nextAxis = gameBoardAxis (marbleHolePosition.axis + 1)
    if (nextAxis = startAxis)
    then newGameBoard
    else
        let nextMarbleHolePosition = toRotatedLaneCoord 1 marbleHolePosition  
        reduceGameBoardAllAxisStates newGameBoard (nextMarbleHolePosition,newMarbleHoleState) startAxis

let reduceGameBoardState (gameBoard:GameBoard) (marbleHolePosition:LaneCoord, newMarbleHoleState) = 
    reduceGameBoardAllAxisStates gameBoard (marbleHolePosition, newMarbleHoleState) marbleHolePosition.axis

let removeGameMarble (gameBoard:GameBoard) (marbleHolePosition:LaneCoord) =
    let i = marbleHolePosition.index
    let r = gameBoardRow marbleHolePosition.row
    let a = marbleHolePosition.axis
    match gameBoard.[a].[r].[i] with
        | Empty -> failwith "Lane hole does not contain a marble!"
        | Marble(marbleColor) -> (reduceGameBoardState gameBoard (marbleHolePosition,Empty)), marbleColor
       
let addGameMarble (gameBoard:GameBoard) (marbleColor:MarbleColor) (marbleHolePosition:LaneCoord) =
    let i = marbleHolePosition.index
    let r = gameBoardRow marbleHolePosition.row
    let a = marbleHolePosition.axis
    match gameBoard.[a].[r].[i] with
        | Empty -> reduceGameBoardState gameBoard (marbleHolePosition, Marble(marbleColor))
        | _ -> failwith "Lane hole already contains a marble!"

let moveGameMarble = removeGameMarble >>* addGameMarble


