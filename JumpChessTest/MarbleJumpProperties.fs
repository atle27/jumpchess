module JumpChessTest.MarbleJumpProperties

open System
open System.Diagnostics
open FsCheck
open FsCheck.Xunit
open JumpChess.Common
open JumpChess.MarbleLane
open JumpChess.MarbleJump
open Common

let printHole (hole:MarbleHole) =
    match hole with
    | Empty -> Debug.Write("x")
    | Marble(color) -> Debug.Write("o")
    
let printLane lane =
    for hole in lane do printHole hole
    Debug.WriteLine ""
    
let rec private emptyHolesInRange (lane:MarbleLane) fromIndex toIndex =
    if fromIndex > toIndex
    then 0
    else
        let step = if toIndex > fromIndex then 1 else -1
        if lane.[fromIndex] = Empty then 1 else 0
        +
        emptyHolesInRange lane (fromIndex+step) toIndex

let isValidJump (isSuperJump:bool) (marbleLane:MarbleLane) (fromIndex:int) (toIndex:int) =
        if (Math.Abs(fromIndex - toIndex) = 1)
        then marbleLane.[toIndex] = Empty
        else
            if not isSuperJump && Math.Abs(fromIndex - toIndex) > 2
            then false
            else
                let direction = if toIndex > fromIndex then 1 else -1
                let jumpHoles = Math.Abs(fromIndex-toIndex)
                let emptyHoles = emptyHolesInRange marbleLane fromIndex toIndex
                let allHolesEmptyButOne = emptyHoles = jumpHoles-1
                let middleHoleNotEmpty = marbleLane.[fromIndex + ((toIndex - fromIndex) / 2)] <> Empty 
                allHolesEmptyButOne && middleHoleNotEmpty

let laneLengthRange = Gen.elements [1..13] |> Arb.fromGen

let percentageRange = Gen.elements [1..100] |> Arb.fromGen

[<Property(QuietOnSuccess = true)>]
let ``All jumps are valid`` () =
    Prop.forAll laneLengthRange <| fun laneLength ->
        Prop.forAll percentageRange <| fun fillPercentage ->
            let marbleCount = laneLength * fillPercentage / 100
            let testLane = addRandomMarblesToLane (buildLane laneLength) marbleCount
            let jumpFromIndex = random.Next (laneLength-1)
            let jumpLane = 
                if (testLane.[jumpFromIndex] = Empty) 
                then addMarble testLane (randomMarbleColor()) jumpFromIndex
                else testLane
            printLane jumpLane |> ignore
            let allJumpToIndices = jumpIndices jumpLane jumpFromIndex false
            allJumpToIndices |> Seq.forall 
                (fun jumpToIndex -> 
                    let isValid = isValidJump false jumpLane jumpFromIndex jumpToIndex
                    isValid)
