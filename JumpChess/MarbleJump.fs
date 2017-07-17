module JumpChess.MarbleJump

open System
open JumpChess.Common
open JumpChess.MarbleLane

let rec internal emptyHoleCount marbleLane laneIndex direction = // consecutive empty hole count
    let ajacentHole = laneIndex + 1 * direction
    if isOutOfBounds marbleLane ajacentHole
    then 0
    else
        match marbleLane.[ajacentHole] with
        | Empty -> 1 * direction + emptyHoleCount marbleLane ajacentHole direction
        | _ -> 0

let rec private superJumpIndices marbleLane marbleIndex jumpDirection =
    seq {
        let beforeJumpEmptyHoleCount = emptyHoleCount marbleLane marbleIndex jumpDirection
        let jumpOverIndex = marbleIndex + beforeJumpEmptyHoleCount + 1 * jumpDirection
        let endEmptyHoleCount = emptyHoleCount marbleLane jumpOverIndex jumpDirection
        if (Math.Abs(endEmptyHoleCount) > Math.Abs(beforeJumpEmptyHoleCount)) 
        then
            let jumpToLocation = marbleIndex + (beforeJumpEmptyHoleCount + 1 * jumpDirection) * 2
            yield jumpToLocation
            yield! superJumpIndices marbleLane jumpToLocation jumpDirection
        else yield! Seq.empty }

let rec private singleJumpIndices marbleLane marbleIndex jumpDirection =
    seq {
        let jumpToIndex = marbleIndex + 2 * jumpDirection
        if isOutOfBounds marbleLane jumpToIndex
        then yield! Seq.empty
        else
            let jumpOverIndex = jumpToIndex - 1 * jumpDirection
            let isEmptyHole marbleLaneIndex = (marbleLane.[marbleLaneIndex] = Empty)
            match isEmptyHole jumpOverIndex, isEmptyHole jumpToIndex with
            | false,true -> 
                yield jumpToIndex
                yield! singleJumpIndices marbleLane jumpToIndex jumpDirection
            | _ -> yield! Seq.empty }
       
let internal jumpIndices marbleLane marbleIndex isSuperJump =
    seq {
        for jumpDirection in [ 1; -1 ] do
            let ajacentIndex = marbleIndex + 1 * jumpDirection
            if isOutOfBounds marbleLane ajacentIndex
            then yield! Seq.empty
            else
                match marbleLane.[ajacentIndex] with
                | Empty ->
                    if not isSuperJump
                    then yield ajacentIndex
                    else
                        yield ajacentIndex
                        yield! superJumpIndices marbleLane marbleIndex jumpDirection
                | _ -> 
                    if isSuperJump
                    then yield! superJumpIndices marbleLane marbleIndex jumpDirection
                    else yield! singleJumpIndices marbleLane marbleIndex jumpDirection }

    
                 

        

