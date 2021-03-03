// Single-line comments look like this.
(*
    Multiline comments look like this.
*)

// There has to be a top-level module.
// We will ignore modules for now and cover them later, in lecture 4.
module DontWorryBeHappy

// Definitions.

// We can make definitions usingwith 'let'.
let x = 5

// Type annotations are optional unless the type is too complicated to infer.
let y : int = 5

// We can give declarations and definitions separately using 'val'.
val z : int
let z = 42

// Recursive functions are defined with 'let rec'.
let rec fib (n : int) : int =
    if n <= 0
    then 0
    else
        if n = 1
        then 1
        else fib (n - 1) + fib (n - 2)

// Functions are first-class citizens.

// We can define anonymous functions.
let f = fun (_ : int) -> 42

// Functions can take other functions as arguments.
let twice f x = f (f x)

// Functions cna be returned as results.
let twice' (f : int -> int) : int -> int =
    fun (n : int) -> f (f n)

// Algebraic data types.

// We can define sum types.
// They are also known as tagged unions, disjoint unions, variants etc.
type intOrString =
    | Left of int
    | Right of string

// Functions on such types are defined by pattern matching.
// Patterns must be exhaustive, i.e. all cases must be matched.
let show (x : intOrString) : string =
    match x with
    | Left _ -> "an integer"
    | Right s -> s

// Another syntax for defining sum types.
// We will use this one more often.
type intOrString' =
    | Left'  : int    -> intOrString'
    | Right' : string -> intOrString'

type prod =
    {
        left  : int;
        right : string;
    }