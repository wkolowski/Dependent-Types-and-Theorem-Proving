module DependentRecords

open FStar.String
open FStar.Squash

// An ordinary pair.
let p : int * string = (42, "42")

// Projections.
let n = fst p
let s = snd p

// We can pattern match on pairs.
let concat (p : int * string) : string =
    match p with
    | (n, s) -> string_of_int n ^ s

// A dependent pair - the TYPE of the second component depends on the
// VALUE of the first component.
let p' : b : bool & (if b then nat else string) =
    (| false, "false" |)

// There are no built-in projections for dependent pairs, but we can
// define them using pattern matching.
let fst' (#a : Type) (#b : a -> Type) (p : (x : a & b x)) : a =
    match p with
    | (| x, y |) -> x

let snd' (#a : Type) (#b : a -> Type) (p : (x : a & b x)) : b (fst' p) =
    match p with
    | (| x, y |) -> y

let third
    (#a : Type) (#b : a -> Type) (#c : (x : a) -> b x -> Type)
    (p : (x : a) & (y : b x) & c x y)
    : c (fst' p) (fst' (snd' p))
    = snd' (snd' p)