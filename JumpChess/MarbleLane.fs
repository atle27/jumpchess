module JumpChess.MarbleLane

open JumpChess.Common
open Chiron

type MarbleColor = | Black | White | Red | Blue | Green | Yellow with
    member x.toString = toString x
    static member fromString s = fromString<MarbleColor> s

type MarbleHole = | Empty | Marble of MarbleColor with
    static member ToJson (marbleHole : MarbleHole) =
        match marbleHole with
        | Empty -> ToJsonDefaults.ToJson "Empty"
        | Marble(color) -> ToJsonDefaults.ToJson color.toString
    static member FromJson (_ : MarbleHole) : Json<MarbleHole> =
        fun json ->
            match json with
            | String "Empty" -> Value Empty, json
            | String "Black" -> Value (Marble(Black)), json
            | String "White" -> Value (Marble(White)), json
            | String "Red" -> Value (Marble(Red)), json
            | String "Blue" -> Value (Marble(Blue)), json
            | String "Green" -> Value (Marble(Green)), json
            | String "Yellow" -> Value (Marble(Yellow)), json
            | s -> Error (sprintf "Expected MarbleHole, found %A" s), json

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



