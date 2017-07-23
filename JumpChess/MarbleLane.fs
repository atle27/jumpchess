module JumpChess.MarbleLane

open JumpChess.Common

type MarbleColor = | Black | White | Red | Blue | Green | Yellow 

type MarbleHole = | Empty | Marble of MarbleColor

type MarbleLane = MarbleHole array

let marbleColors = [| Black; White; Red; Blue; Green; Yellow |]

let internal buildLane holeCount = 
    Array.create holeCount Empty : MarbleLane

let internal buildLanes holeCounts = 
    holeCounts |> Seq.map buildLane

let internal isOutOfBounds (lane:MarbleLane) index = 
    index < 0 || index >= Array.length lane 

let internal marbleLaneWithNewState (marbleLane:MarbleLane) (marbleHoleIndex, marbleHoleNewState) =
    let newMarbleLane = Array.copy marbleLane
    Array.set newMarbleLane marbleHoleIndex marbleHoleNewState
    (newMarbleLane:MarbleLane)

let internal removeMarble (lane:MarbleLane) index =
    match lane.[index] with
    | Empty -> failwith "Lane hole does not contain a marble!"
    | Marble(marbleColor) -> marbleLaneWithNewState lane (index, Empty), marbleColor
        
let internal addMarble (lane:MarbleLane) marbleColor index =
    match lane.[index] with
    | Empty -> marbleLaneWithNewState lane (index, Marble(marbleColor))
    | _ -> failwith "Lane hole already contains a marble!"
   
let internal moveMarble = removeMarble >>* addMarble



