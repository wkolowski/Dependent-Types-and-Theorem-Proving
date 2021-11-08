module Vectors

// Recursive definition of vec.
let rec recursiveVec (a : Type) (n : nat) : Type =
    if n = 0
    then unit
    else a * recursiveVec a (n - 1)

// Inductive definition of vec.
type inductiveVec (a : Type) : nat -> Type =
    | VNil  : inductiveVec a 0
    | VCons : a -> (#n : nat) -> inductiveVec a n -> inductiveVec a (1 + n)

// fin n is the type of numbers between 0 and n - 1.
type fin : nat -> Type =
    | FinZ : (#n : nat) -> fin (1 + n)
    | FinS : (#n : nat) -> fin n -> fin (1 + n)

// Nonrecursive definition of vec.
let nonrecursiveVec (a : Type) (n : nat) : Type =
    fin n -> a

open FStar.List

// vec as a list paired with proof that its length is n
let extrinsicVec (a : Type) (n : nat) : Type =
    l : list a & (length l = n)