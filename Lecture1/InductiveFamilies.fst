module InductiveFamilies

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
// simpler languages like F# or Haskell, the solution is again to
// use an option.

let rec nth (#a : Type) (n : nat) (l : myList a) : myOption a =
    match l with
    | MyNil      -> MyNone
    | MyCons h t -> if n = 0 then MySome h else nth (n - 1) t



// Our option-based solutions are nice, but can we do better? The answer is
// yes, provided we have dependent types, and inductive families in particular
// are a perfect for this purpose.

// We can solve our problem by defining a type (family) of lists indexed by
// their length (such a family of types is traditionally called 'Vec', which
// stands for "Vector"). With such a type, we can allow taking the head and
// indexing of only those vectors, which are long enough to guarantee that an
// element with the requested index is present.

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
// length 1 + n. If we tried to call this 'vhead' with a vector of length
// 0 as argument, it wouldn't typecheck.

// Note that we don't need to handle all cases in the pattern match. The case
// 'VNil' cannot occur, because that would mean the index of v is 0, but we
// know (and F* knows too) that vectors of length 0 are not allowed as inputs.

type fin : nat -> Type =
    | FinZ : (#n : nat) -> fin (1 + n)
    | FinS : (#n : nat) -> fin n -> fin (1 + n)

// We can approach the problem of defining a safe indexing function for
// vectors by first defining 'fin', a family of finite types. Our intention
// is that fin n is the type that has precisely n ditinct values. This is
// achieved with two constructors: fin0 says that the type fin (1 + n) has
// an element, whereas the constructor finS says that the type fin (1 + n)
// has an element if fin n, the type with n elements, has an element.

let rec vnth (#a : Type) (#n : nat) (i : fin (1 + n)) (v : vec a (1 + n)) : a =
    match i, v with
    | FinZ   , VCons h _ -> h
    | FinS i', VCons _ t -> vnth i' t
    
// Example: enforcing protocols
// Look up Idris tutorials/manuals/books for some example
// on how to use inductive families for enforcing protocols,
// i.e. for using sockets.