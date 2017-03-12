module JumpChess.MarbleLane

open JumpChess.Common

type MarbleColor = | Black | White | Red | Blue | Green | Yellow 

type MarbleHole = | Empty | Marble of MarbleColor

type MarbleLane = MarbleHole array

let private reduce (marbleLane:MarbleLane) (index, newMarbleHole) =
    let newMarbleLane = Array.copy marbleLane
    Array.set newMarbleLane index newMarbleHole
    (newMarbleLane:MarbleLane)

let removeMarble (lane:MarbleLane) index =
    match lane.[index] with
    | Empty -> failwith "Lane hole does not contain a marble!"
    | Marble(marbleColor) -> reduce lane (index, Empty), marbleColor
        
let addMarble (lane:MarbleLane) marbleColor index =
    match lane.[index] with
    | Empty -> reduce lane (index, Marble(marbleColor))
    | _ -> failwith "Lane hole already contains a marble!"
   
let moveMarble = removeMarble >>* addMarble

let isOutOfBounds (lane:MarbleLane) index = 
    index < 0 || index >= Array.length lane 

let buildLane holeCount = 
    Array.create holeCount Empty : MarbleLane

let buildLanes holeCounts = 
    holeCounts |> Seq.map buildLane


