module InductiveFamilies

// 1. Refresher on inductive types.

// Let's once more recall how to define lists - an ordinary inductive
// type. Note: we will name it 'myList' in order to avoid name clashes
// with the standard library.

type myList (a : Type) : Type =
    | MyNil  : myList a
    | MyCons : a -> myList a -> myList a

// Inductive types are nice, but there are some problems with them.
// When it comes to lists, one of the most obvious examples is the
// function 'head', which accesses the first element of the list...
// but what if the first element is not there, i.e. what if the
// list is empty? In languages like F# or Haskell we could throw an
// exception, but F* doesn't allow this, so we need to use the option
// type as return type instead.

type myOption (a : Type) : Type =
    | MyNone : myOption a
    | MySome : a -> myOption a

let head (#a : Type) (l : myList a) : myOption a =
    match l with
    | MyNil      -> MyNone
    | MyCons h _ -> MySome h

// That's a nice solution, but a bit annoying from time to time,
// because our result gets wrapped in an option even if we know
// that the list is not empty. Can we do better? Before we answer,
// note that the same problem appears not only for the head of the
// list, but in general when trying to access the n-th element. In
// simpler languages, like F# or Haskell, we could throw an "index
// out of bounds" error or something similar, but in F* the solution
// is again to use an option.

let rec get (#a : Type) (l : myList a) (i : nat) : myOption a =
    match l with
    | MyNil      -> MyNone
    | MyCons h t -> if i = 0 then MySome h else get t (i - 1)



// 2. Inductive families.

// 2.1. Vectors.

// Our option-based solutions are nice, but can we do better? The answer is
// yes, provided we have dependent types, and inductive families in particular
// are a perfect for this purpose.

// We can solve our problem by defining a type family of lists indexed by
// their length. Such a family of types is traditionally called 'Vec', which
// stands for "Vector". With Vec, we can allow taking the head and indexing
// of only those vectors, which are long enough to guarantee that an element
// with the requested index is present.

type vec (a : Type) : nat -> Type =
    | VNil  : vec a 0
    | VCons : a -> (#n : nat) -> vec a n -> vec a (1 + n)

// Notice a few details in the above definition. First, the type of the
// whole thing is nat -> Type, which indicates that we want to define a
// family of types indexed by natural numbers (i.e. the positive integers).
// Second, notice that the index is different in different constructors:
// 'VNil' has index 0, which we should understand as an encoding of the fact
// that the empty vector has length 0. 'VCons', on the other hand, when given
// a value of type a and a vector of length n, creates a vector of length
// 1 + n.

let vhead (#a : Type) (#n : nat) (v : vec a (1 + n)) : a =
    match v with
    | VCons h _ -> h

// Now we can define a nice function that retrieves the first element in
// a vector, provided that its length is greater than zero. This is the
// crux of the matter: the only valid arguments to 'vhead' are vectors of
// length 1 + n. If we tried to call 'vhead' with a vector of length 0 as
// argument, it wouldn't typecheck.

// Note that we don't need to handle all cases in the pattern match. The case
// 'VNil' cannot occur, because that would mean the index of v is 0, but we
// know (and F* knows too) that only vectors of length 1 + n are allowed as
// inputs.

type fin : nat -> Type =
    | FinZ : (#n : nat) -> fin (1 + n)
    | FinS : (#n : nat) -> fin n -> fin (1 + n)

// We can approach the problem of defining a safe indexing function for
// vectors by first defining 'fin', a family of finite types. Our intention
// is that fin n is the type that has precisely n distinct values.

// This is achieved with two constructors: FinZ says that the type fin (1 + n)
// has an element, whereas the constructor FinS says that the type fin (1 + n)
// has an element if fin n, the type with n elements, has an element.

// Now, we see that 'vec a n' is a vector of length n, whereas there are
// precisely n values of type 'fin n' (we can think that these values are
// numbers 0 through n - 1, represented in unary). This allows us to solve
// our problem, because there are as many elements in the vector as there are
// possible ways to index the vector.

let rec vget (#a : Type) (#n : nat) (v : vec a n) (i : fin n)  : a =
    match v with
    | VNil      -> () // SMT solver magic.
    | VCons h t ->
        match i with
        | FinZ    -> h
        | FinS i' -> vget t i'

// The indexing function works as follows: we take as arguments a vector
// of length n called v and an index i of type fin n. We match on v. In
// case its empty, some automated proof magic happens. Because v matched
// VNil, we know that n must be 0, so the index i is of type fin 0. But
// we know that there are no elements of type fin 0, because both constructors
// return elements with a positive index. So we have something that shouldn't
// exist - this is a contradiction, and F* can find this contradiction for us
// automatically.

// Note that the '()' here is NOT the same thing as () : unit, the only element
// of the unit type. Here it is a means of invoking F* automated proof finding/
// program synthesis capabilities. Basically, it's like saying "Hey F*, figure
// out what should go there all by yourself".

// In the second case, when v matches 'VCons h t', we match the index i. If its
// 0 (represented by FinZ), we return the head of the vector. Otherwise, we keep
// looking in the vector's tail t with an index decremented by 1 (FinS i' can
// be interpreted as 1 + i').