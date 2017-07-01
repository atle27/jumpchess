module JumpChessTest.GameBoardTests

open System
open Xunit
open FsCheck
open FsCheck.Xunit
open JumpChess.Common
open JumpChess.GameBoard
open JumpChessTest.Common

[<Fact>]
let ``Any of the game board lanes have the same length as the lanes one the real physical board`` () =
    Assert.True(gameBoard.[2].[8].Length = 9)
    Assert.True(gameBoard.[1].[2].Length = 3)
    Assert.True(gameBoard.[0].[5].Length = 12)

[<Fact>]
let ``Verify correct conversion from board origin to lane coords`` () =
    let boardCoord = { x = 0; y = 0; rot = -4 }
    let landCoordExpected = { index = 4; row = 0; rot = 1 }
    let laneCoordActual = toLaneCoord boardCoord
    Assert.Equal(laneCoordActual, landCoordExpected)

[<Fact>]
let ``Verify correct conversion from board coords to lane coords`` () =
    let boardCoord = { x = -8; y = 4; rot = -4 }
    let landCoordExpected = { index = 3; row = 2; rot = 1 }
    let laneCoordActual = toLaneCoord boardCoord
    Assert.Equal(laneCoordActual, landCoordExpected)

[<Fact>]
let ``Verify correct conversion from lane coords to board coords`` () =
    let laneCoord = { index = 0; row = -5; rot = -5 }
    let boardCoordExpected = { x = -8; y = -10; rot = 2 }
    let boardCoordActual = toBoardCoord laneCoord
    Assert.Equal(boardCoordActual, boardCoordExpected)

let boardAxisRange = Gen.elements [0..2] |> Arb.fromGen
let boardRowRange = Gen.elements [-8..8] |> Arb.fromGen

[<Property>]
let ``When converting between lane and board coordinates the rotation does not change`` () =
    Prop.forAll boardAxisRange <| fun axis ->
        Prop.forAll boardRowRange <| fun row ->
            let laneCoord = { index = 0; row = row; rot = axis }
            let boardCoord = toBoardCoord laneCoord
            Assert.True(laneCoord.rot = boardCoord.rot)

[<Property>]
let ``When converting from lane coord. to board coord. and back we should arrive at the initial location`` () =
    Prop.forAll boardAxisRange <| fun axis ->
        Prop.forAll boardRowRange <| fun row ->
            let lane = gameBoard.[axis].[gameLaneRowIndex row]
            let laneIndex = randomMarbleLaneIndex lane
            let laneCoord = { index = laneIndex; row = row; rot = axis }
            let boardCoord = toBoardCoord laneCoord
            let laneCoord2 = toLaneCoord boardCoord
            Assert.Equal(laneCoord, laneCoord2)


    
