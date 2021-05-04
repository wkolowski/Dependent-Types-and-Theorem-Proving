module InductiveFamilies







type myList (a : Type) : Type =
    | MyNil  : myList a
    | MyCons : a -> myList a -> myList a









type myOption (a : Type) : Type =
    | MyNone : myOption a
    | MySome : a -> myOption a

let head (#a : Type) (l : myList a) : myOption a =
    match l with
    | MyNil      -> MyNone
    | MyCons h _ -> MySome h










let rec get (#a : Type) (l : myList a) (i : nat) : myOption a =
    match l with
    | MyNil      -> MyNone
    | MyCons h t -> if i = 0 then MySome h else get t (i - 1)

















type vec (a : Type) : nat -> Type =
    | VNil  : vec a 0
    | VCons : a -> (#n : nat) -> vec a n -> vec a (1 + n)










let vhead (#a : Type) (#n : nat) (v : vec a (1 + n)) : a =
    match v with
    | VCons h _ -> h












type fin : nat -> Type =
    | FinZ : (#n : nat) -> fin (1 + n)
    | FinS : (#n : nat) -> fin n -> fin (1 + n)















let rec vget (#a : Type) (#n : nat) (v : vec a n) (i : fin n)  : a =
    match v with
    | VNil      -> () 
    | VCons h t ->
        match i with
        | FinZ    -> h
        | FinS i' -> vget t i'