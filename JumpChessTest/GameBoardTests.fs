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
    let x = gameBoard.[0].[5].Length
    Assert.True(gameBoard.[0].[5].Length = 12)
   
let boardRotRange = Gen.elements [0..2] |> Arb.fromGen
let boardRowRange = Gen.elements [-8..8] |> Arb.fromGen

[<Property>]
let ``When converting between lane and board coordinates the rotation does not change`` () =
    Prop.forAll boardRotRange <| fun rot ->
        Prop.forAll boardRowRange <| fun row ->
            let laneCoord = { index = 0; row = row; rot = rot }
            let boardCoord = toBoardCoord laneCoord
            Assert.True(laneCoord.rot = boardCoord.rot)
    
