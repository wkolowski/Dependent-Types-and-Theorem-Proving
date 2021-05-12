module Test

let x1 : bool = not true
let x2 : bool = x1 && x1
let x3 : bool = x1 || x2
let x4 : bool = x2 ==> x3
let x5 : bool = x3 <=> x4
