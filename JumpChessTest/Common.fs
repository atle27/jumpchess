module JumpChessTest.Common

open System
open JumpChess.MarbleLane

#nowarn "40" // the recusive infinte sequence do not need a soundness runtime check!

let random = Random()

let randomUnionCase<'t>() = 
  let cases = Reflection.FSharpType.GetUnionCases(typeof<'t>)
  let index = random.Next cases.Length
  let case = cases.[index]
  Reflection.FSharpValue.MakeUnion(case, [||]) :?> 't

let randomMarbleColor () = randomUnionCase<MarbleColor>()

let randomMarbleColors () =
    let rec randomColors = seq { 
        let randomColor = randomMarbleColor ()
        yield randomColor
        yield! randomColors }
    randomColors

let rec addRandomMarblesToLane (marbleLane:MarbleLane) (marbleCount:int) =
    let index = random.Next (marbleLane.Length-1)
    if (marbleCount = 0)
    then marbleLane
    else
        match marbleLane.[index] with
        | Empty -> addRandomMarblesToLane (addMarble marbleLane (randomMarbleColor()) index) (marbleCount-1)
        | _ -> addRandomMarblesToLane marbleLane marbleCount

