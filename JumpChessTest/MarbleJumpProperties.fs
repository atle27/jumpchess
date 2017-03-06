module JumpChessTest.MarbleJumpProperties

open System
open FsCheck
open FsCheck.Xunit
open JumpChess.Common
open JumpChess.MarbleLane
open JumpChess.MarbleJump
open Common

let isValidSuperJump (marbleLane:MarbleLane) (fromIndex:int) (toIndex:int) =
        if (Math.Abs(fromIndex - toIndex) = 1)
        then marbleLane.[toIndex] = Empty
        else
            let direction = if toIndex > fromIndex then 1 else -1
            let allHolesEmptyButOne = (emptyHoleCount marbleLane fromIndex direction) = Math.Abs(fromIndex-toIndex)-1
            let middleIndexNotEmpty = marbleLane.[fromIndex + ((fromIndex - toIndex) / 2)] <> Empty 
            allHolesEmptyButOne && middleIndexNotEmpty

let laneLengthRange = Gen.elements [1..13] |> Arb.fromGen

let percentage = Gen.elements [1..100] |> Arb.fromGen

[<Property>]
let ``All jumps are valid`` () =
    Prop.forAll laneLengthRange <| fun laneLength ->
        Prop.forAll percentage <| fun laneFillPercentage ->
            let marbleCount = laneLength * laneFillPercentage / 100
            let testLane = addRandomMarblesToLane (buildLane laneLength) marbleCount
            let jumpFromIndex = random.Next (laneLength-1)
            let jumpLane = 
                if (testLane.[jumpFromIndex] = Empty) 
                then addMarble testLane (randomMarbleColor()) jumpFromIndex
                else testLane
            let allJumpToIndices = jumpIndices jumpLane jumpFromIndex true
            allJumpToIndices |> Seq.forall (fun jumpToIndex -> isValidSuperJump testLane jumpFromIndex jumpToIndex)
