module Universe

// Type abbreviation mechanism.
type integer = int

let add_integer (n m : integer) : integer = n + m



// Separate type abbreviation mechanism is not needed - let bindings are enough
// because types are first class.
let integer' = int

let add_integer' (n m : integer') : integer = n + m

let integer'' : Type = int

// We can store types in data structures.
let listOfTypes : list Type =
    [int; string; int -> int; string * string; list int; list (list int)]

// We can return types from functions.
let listOfTriples (a : Type) : Type = list (a * a * a)

let x : listOfTriples int = [(-1, 0, 1); (-2, 0, 2); (-3, 0, 3)]

// We can compute types at runtime by recursion.
let rec vec (a : Type) (n : nat) : Type =
    if n = 0
    then unit
    else a * vec a (n - 1)

let example_vec : vec nat 4 =
    (0, (1, (2, (3, ()))))

let matrix (a : Type) (n m : nat) : Type =
    vec (vec a m) n

let idmatrix3by3 : matrix int 3 3 =
    ((1, (0, (0, ()))),
    ((0, (1, (0, ()))),
    ((0, (0, (1, ()))),
    ())))