module Refinements

// We can refine base types, which is very handy.
let btwn_0_10 : Type =
    x : int{0 <= x && x <= 10}

let x : btwn_0_10 = 5

[@@ expect_failure]
let x' : btwn_0_10 = 14



// We can refine products and records.
type currency = | USD | EUR | PLN

type small_money =
{
    cur : currency;
    amount : btwn_0_10;
}

let only_euros : m : small_money{m.cur = EUR} =
{
    cur = EUR;
    amount = 10;
}

[@@ expect_failure]
let dollars_bad : m : small_money{m.cur = EUR} =
{
    cur = USD;
    amount = 5;
}



// We can refine inductive types.
let nonempty (#a : Type) (l : list a) : bool =
    match l with
        | [] -> false
        | _  -> true

let safe_head (#a : Type) (l : list a {nonempty l}) : a =
    match l with
        | []     -> () // SMT magic.
        | h :: _ -> h

// In fact, F* generates refinements for constructors for us for free.
let safe_head' (#a : Type) (l : list a {Cons? l}) : a =
    match l with
        | []     -> () // SMT magic.
        | h :: _ -> h



open FStar.List

// We can define vec as lists paired with a proof that the length is equal to n.
let vec (a : Type) (n : nat) : Type =
    l : list a {length l = n}



// Refinements are powerful enough to prove some properties of recursive functions.
let rec map (#a #b : Type) (f : a -> b) (l : list a) :
    r : list b {length r = length l} =
    match l with
        | []     -> []
        | h :: t -> f h :: map f t