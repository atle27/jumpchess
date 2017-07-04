module JumpChessTest.MarbleJumpTests

open System
open System.Diagnostics
open FsCheck
open FsCheck.Xunit
open JumpChess.Common
open JumpChess.MarbleLane
open JumpChess.MarbleJump
open Common

let private subLane (lane:MarbleLane) (fromIndex:int) (toIndex:int) =
    Array.sub lane (Math.Min(fromIndex,toIndex)) (Math.Abs(fromIndex-toIndex)+1)

let rec private emptyHolesInRange lane fromIndex toIndex =
    subLane lane fromIndex toIndex 
    |> Array.fold (fun count hole -> if hole = Empty then count + 1 else count) 0

let private isValidSingleSuperJump (marbleLane:MarbleLane) (fromIndex:int) (toIndex:int) (isInter:bool) =
    let jumpHoles = Math.Abs(fromIndex-toIndex)
    let subtractEmptyFromHole = if isInter then -1 else 0
    let emptyHoles = subtractEmptyFromHole + emptyHolesInRange marbleLane fromIndex toIndex
    let allHolesEmptyButOne = emptyHoles = jumpHoles-1
    let middleHoleNotEmpty = marbleLane.[fromIndex + ((toIndex - fromIndex) / 2)] <> Empty 
    allHolesEmptyButOne && middleHoleNotEmpty

let rec private isValidSuperJump (marbleLane:MarbleLane) (fromIndex:int) (toIndex:int) (isInter:bool) =
    let direction = if toIndex > fromIndex then 1 else -1
    let emptyHoles = emptyHoleCount marbleLane fromIndex direction
    let interIndex = fromIndex + (emptyHoles + 1) * 2
    let isValidFirstLeg = isValidSingleSuperJump marbleLane fromIndex interIndex isInter
    let isValidRestLegs = interIndex = toIndex || isValidSuperJump marbleLane interIndex toIndex true
    isValidFirstLeg && isValidRestLegs

let rec isValidJump (isSuperJump:bool) (marbleLane:MarbleLane) (fromIndex:int) (toIndex:int) =
    if (Math.Abs(fromIndex - toIndex) = 1)
    then marbleLane.[toIndex] = Empty
    else
        let direction = if toIndex > fromIndex then 1 else -1
        if isSuperJump
        then
            isValidSuperJump marbleLane fromIndex toIndex false
        else
            let jumpArray = subLane marbleLane fromIndex toIndex
            if direction < 0 then Array.rev jumpArray else jumpArray
            |> Array.mapi (fun index hole -> 
                if index = 0 || isOdd index
                then hole <> Empty
                else hole = Empty)
            |> Array.forall (fun x -> x)

let laneLengthRange = Gen.elements [1..13] |> Arb.fromGen

let percentageRange = Gen.elements [1..100] |> Arb.fromGen

[<Property>]
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
            let isValidJumps isSuperJump =
                let allJumpToIndices = jumpIndices jumpLane jumpFromIndex isSuperJump
                allJumpToIndices |> Seq.forall 
                    (fun jumpToIndex -> 
                        let isValid = isValidJump isSuperJump jumpLane jumpFromIndex jumpToIndex
                        isValid)
            let testValidSuperJumps = isValidJumps true
            let testValidSingleJumps = isValidJumps false
            testValidSuperJumps && testValidSingleJumps

            
