module JumpChess.Common

let curry f a b = f (a,b)
let uncurry f (a,b) = f a b

let (>>*) f g = curry(uncurry(f) >> uncurry(g))

let isOdd n = (n % 2 = 1)


