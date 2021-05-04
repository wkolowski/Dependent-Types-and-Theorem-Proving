// Single-line comments look like this.
(*
    Multiline comments look like this.
*)

// There has to be a top-level module.
// We will ignore modules for now and cover them later, in lecture 4.
module DontWorryBeHappy

// Definitions.

// We can make definitions using 'let'.
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

// Functions can be returned as results.
let twice' (f : int -> int) : int -> int =
    fun (n : int) -> f (f n)

// Sum types / discriminated unions.

// We can define sum types (also known as tagged unions, disjoint unions,
// discriminated unions, variants, etc.).
// Note: 'sum types' is the standard name used in academia and functional
// programming circles. It comes from the fact that if type A has n elements
// and type B has m elements, then their sum A + B has n + m elements (here
// A + B denotes a type defined similarly to intOrString).
type intOrString =
    | Left of int
    | Right of string

// We can define some values.
let an_int : intOrString = Left 42
let a_string : intOrString = Right "forty two"

// Functions on such types are defined by pattern matching.
// Patterns must be exhaustive, i.e. all cases must be matched.
let show (x : intOrString) : string =
    match x with
    | Left _ -> "an integer"
    | Right s -> s

// Left and Right are called the constructors of intOrString.
// Left has type int -> intOrString
// Right has type string -> intOrString
let l : int -> intOrString = Left
let r : string -> intOrString = Right

// Another syntax for defining sum types, which we will use more often than
// the previous one. Here we specify the types of constructors instead of just
// specifying their arguments.
type intOrString' =
    | Left'  : int    -> intOrString'
    | Right' : string -> intOrString'

// Standard library provides us with product types. Their values are pairs,
// sometimes also called tuples. Functions which return components of the
// pair (here 'fst' and 'snd') are called projections.
let swap (p : int * string) : string * int =
    (snd p, fst p)

// However, we can also use pattern matching.
let swap' p =
    match p with
    | (x, y) -> (y, x)

// This can be done even better with an 'irrefutable pattern' (a pattern
// that always matches) which can be put right into argument position.
let swap'' (x, y) = (y, x)

// Note a sad quirk: because * is used for product types, it can't be 
// used for multiplication of numbers. This won't bother us, however.

// A close relative of product types are record types.
type stuff =
{
    num : int;
    str : string;
    bl  : bool;
}

// Records can be created like this:
let someStuff : stuff =
{
    num = 42;
    str = "Hello there!";
    bl  = false;
}

// Record fields can be accessed with dot syntax 'record.field'.
let double_num (s : stuff) : int = s.num + s.num

// Pattern matching is also possible, but not very pretty.
// In fact, it exposes the fact that records are under the hood implemented
// as single-constructor sums.
let double_num' (s : stuff) : int =
    match s with
    | Mkstuff num str bl -> num + num

// The same record type implemented manually.
type stuff' =
    | Mkstuff' : int -> string -> bool -> stuff'

let num' (Mkstuff' i _ _) = i
let str' (Mkstuff' _ s _) = s
let bl'  (Mkstuff' _ _ b) = b

// Note that in the above definition, the constructor has type
// 'int -> string -> bool -> stuff'. Arrows associate to the right,
// so this should be read as 'int -> (string -> (bool -> stuff')))'.

// Thus it's a function that takes an int and returns a function which
// takes a string and returns a function which takes a bool and
// returns stuff'.

// The same thing can be expressed with the type 'int * string * bool -> stuff'.
// Transformations between these two equivalent types are called currying
// and uncyrrying.
// In F*, curried functions are preferred to uncurried ones.
let curry (f : int * string * bool -> stuff') : int -> string -> bool -> stuff' =
    fun i s b -> f (i, s, b)

let uncurry (f : int -> string -> bool -> stuff') : int * string * bool -> stuff' =
    fun p ->
        match p with
        | (i, s, b) -> f i s b

// Inductive types / algebraic data types / discriminated unions (?)

// Algebraic data types are sums of products in which the type being defined
// can appear. Here the constructor Node takes as arguments two trees, i.e.
// values of the type were defining.
type binaryTree =
    | Empty : binaryTree
    | Node  : int -> binaryTree -> binaryTree -> binaryTree

let a_tree : binaryTree =
    Node 1000
        (Node 500 Empty Empty)
        (Node 2000
            (Node 1500 Empty Empty)
            Empty

// Algebraic data types are processed with pattern matching and recursion.
// Remember about the keyword 'let rec'!
let rec mirror (t : binaryTree) : binaryTree =
    match t with
    | Empty -> Empty
    | Node value left right -> Node value (mirror right) (mirror left)