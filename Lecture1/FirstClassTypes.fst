module FirstClassTypes

// We will explain how first-class types work in F*. To do this, we will show
// that F* types satisfy all our first-class-ness criteria.

// In F*, just as in F#, we can create a type alias (also called type synonym or
// type abbreviation). We can do this with the keyword 'type'.
type integer = int

// An alias is just a new name for an old type.
let add_integer (n m : integer) : int = n + m

// But in F* types are first-class, so we can do the same using the plain old
// 'let'. In other words, we can bind types to variables.
let integer' = int

let add_integer' (n m : integer') : int = n + m

// But what is the type of integer'? In F*, the universe of types is called Type.
let integer'' : Type = int

// Another of our criteria, probably the most boring one, was that types
// can be stored in data structures. In F*, they can.
let listOfTypes : list Type =
    [int; string; int -> int; string * string; list int; list (list int)]

// Another first-class-ness criteria was that types can be passed to
// functions as arguments and returned from functions. In F* they can.
let listOfTriples (a : Type) : Type = list (a * a * a)

let x : listOfTriples int = [(-1, 0, 1); (-2, 0, 2); (-3, 0, 3)]

// But the most mind-blowing feature of first-class types (well, at least for
// ordinary people) is the ability to compute with types at runtime, which
// was also one of our criteria.

// Let's see how we can implement the type of n by m matrices we saw in one
// of the examples.

// First, we can define a type of lists of a given length, which we will
// call a vector.
let rec vec (a : Type) (n : nat) : Type =
    if n = 0
    then unit
    else a * vec a (n - 1)

// A matrix is just a vector of vectors.
let matrix (a : Type) (n m : nat) : Type =
    vec (vec a m) n

// Now we can define a 3 by 3 identity matrix. Sadly, the matrices look pretty
// ugly.
let idmatrix3by3 : matrix int 3 3 =
    ((1, (0, (0, ()))),
    ((0, (1, (0, ()))),
    ((0, (0, (1, ()))),
    ())))