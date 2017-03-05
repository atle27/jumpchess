module JumpChessTest.MarbleLaneTests

open System
open Xunit
open JumpChess.Common
open JumpChess.MarbleLane
open JumpChessTest.Common

[<Fact>]
let ``When removing a marble from an empty lane it throws exception`` () =
    let testLane = buildLane 8
    Assert.Throws<System.Exception>(
        fun () -> removeMarble testLane 3 |> ignore)

[<Fact>]
let ``When adding marbles twice at same hole it throws exception`` () =
    let testLane = buildLane 8
    let testHole = 3
    Assert.Throws<System.Exception>(
        fun () -> addMarble (addMarble testLane (randomMarbleColor()) testHole) (randomMarbleColor()) testHole |> ignore)

[<Fact>]
let ``When adding and removing same marble the hole first get a marble of correct color then the same colored marble gets removed and leaves hole empty`` () =
    let testColor = randomMarbleColor()
    let testLane = buildLane 8
    let testHole = 3
    let laneWithAdded = addMarble testLane testColor testHole
    Assert.True(laneWithAdded.[testHole] = Marble(testColor))
    let laneWithRemoved, removedMarble = removeMarble laneWithAdded testHole
    Assert.True((removedMarble = testColor))
    Assert.True(laneWithRemoved.[testHole] = Empty)

[<Fact>]
let ``When moving a marble it give a new lane with the source hole empty and the target hole with a marble of correct color`` () =
    let testColor = randomMarbleColor()
    let sourceHole = 4
    let targetHole = 6
    let testLane = addMarble (buildLane 8) testColor sourceHole
    let newLane = moveMarble testLane sourceHole targetHole
    Assert.True(newLane.[sourceHole] = Empty)
    Assert.True(newLane.[targetHole] = Marble(testColor)) 

[<Fact>]
let ``A hole index at half the lane length is not out of bounds`` () =
    let lane1 = buildLane 8
    Assert.False(isOutOfBounds lane1 4)

[<Fact>]
let ``A hole index of lane length or -1 is out of bounds`` () =
    let lane1 = buildLane 8
    Assert.True(isOutOfBounds lane1 -1)
    Assert.True(isOutOfBounds lane1 8)


