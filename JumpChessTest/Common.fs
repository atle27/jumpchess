module JumpChessTest.Common

open JumpChess.MarbleLane

#nowarn "40" // the recusive infinte sequence do not need a soundness runtime check!
  
let randomUnionCase<'t>() = 
  let cases = Reflection.FSharpType.GetUnionCases(typeof<'t>)
  let index = System.Random().Next(cases.Length)
  let case = cases.[index]
  Reflection.FSharpValue.MakeUnion(case, [||]) :?> 't

let randomMarbleColor () = randomUnionCase<MarbleColor>()

let randomMarbleColors () =
    let rec randomColors = seq { 
        let randomColor = randomMarbleColor ()
        yield randomColor
        yield! randomColors }
    randomColors

