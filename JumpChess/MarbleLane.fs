module JumpChess.MarbleLane

open JumpChess.Common

type MarbleColor = | Black | White | Red | Blue | Green | Yellow 

type MarbleHole = | Empty | Marble of MarbleColor

type MarbleLane = MarbleHole array

let buildLane holeCount = 
    Array.create holeCount Empty : MarbleLane

let buildLanes holeCounts = 
    holeCounts |> Seq.map buildLane

let isOutOfBounds (lane:MarbleLane) index = 
    index < 0 || index >= Array.length lane 

let internal reduceMarbleLaneState (marbleLane:MarbleLane) (marbleHoleIndex, newMarbleHoleState) =
    let newMarbleLane = Array.copy marbleLane
    Array.set newMarbleLane marbleHoleIndex newMarbleHoleState
    (newMarbleLane:MarbleLane)

let removeMarble (lane:MarbleLane) index =
    match lane.[index] with
    | Empty -> failwith "Lane hole does not contain a marble!"
    | Marble(marbleColor) -> reduceMarbleLaneState lane (index, Empty), marbleColor
        
let addMarble (lane:MarbleLane) marbleColor index =
    match lane.[index] with
    | Empty -> reduceMarbleLaneState lane (index, Marble(marbleColor))
    | _ -> failwith "Lane hole already contains a marble!"
   
let moveMarble = removeMarble >>* addMarble



