module JumpChess.Common

open Microsoft.FSharp.Reflection

let curry f a b = f (a,b)
let uncurry f (a,b) = f a b

let (>>*) f g = curry(uncurry(f) >> uncurry(g))

let isOdd n = (n % 2 = 1)

let toString (x:'a) = 
    match FSharpValue.GetUnionFields(x, typeof<'a>) with
    | case, _ -> case.Name

let fromString<'a> (s:string) =
    match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with
    |[|case|] -> FSharpValue.MakeUnion(case,[||]) :?> 'a
    |_ -> failwith "Unable to convert string to disciminated union"