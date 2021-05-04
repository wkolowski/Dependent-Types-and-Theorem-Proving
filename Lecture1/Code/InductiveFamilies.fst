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



// 3. Enforcing protocols in types.

// With indexed families, whenever we use a value of type a i1 (for some type
// family a : i -> Type and index i1 : i) in place where a value of type a i2
// (for some i2 : i) is expected, the typechecker has to check whether i1 and
// i2 are equal to make sure that the program is well-typed.

// Sounds completely boring? Well, this gives us A LOT of new opportunities,
// because we can use this little check of index equality  to detect (and
// rule out) buggy programs! We just need to encode the relevant correctness
// criteria in the indices of our type families.

// There can be various situations to which this technique applies, but the
// most obvious one is when we are dealing with state machines, i.e: there
// is an object which can be in a bunch of states and we have some operations
// which, in addition to doing something useful, can change the state of the
// object. Some of these operations are also valid only in certain states and
// invalid in others. The problem that we want to solve is: how to represent
// this situation in code with as few bugs as possible?

// The classical solution is described here:
// https://fsharpforfunandprofit.com/posts/designing-with-types-representing-states/

// But the most important insight for us is this: define a type of states and
// then define a type family indexed by states, which will represent the
// stateful objects. When defining operations on the stateful objects, we can
// explicitly track all state transitions and other state-related criteria in
// the indices.

// Let's see an example. We want to model a data store from which data can be
// retrieved. Everybody can retrieve public data, but there's also some top
// secret data which can be retrieved only by users who are logged in.

// The state the we are concerned with is being 'logged in' or 'logged out'.
// We model this state as a simple type with two values.
type loginState =
    | LoggedIn
    | LoggedOut

// The stateful object is the data store - what the user can do with it depends
// on whether he is logged in or not. So, we model data stores as a family of
// types indexed by loginState.
assume type dataStore : loginState -> Type

// Note: in this example we don't care about the particular definitions of
// types and functions, we will just declare them using 'assume type' and
// 'assume val'.
assume type data

// We can now model data access privileges using indices. To read public data,
// a data store in any state s suffices.
assume val readPublicData    : (#s : loginState) -> dataStore s -> data

// But to read top secret data, the user needs a data store for which he is
// logged in - he can't log in if he is already logged in. This invariant is
// guaranteed by the index of the argument.
assume val readTopSecretData : dataStore LoggedIn -> data

// We also need to handle logging in and logging out.

// Logging out of a data store is simple: it requires that the user is already
// logged in and results in a store for which the user is logged out.
assume val logout : dataStore LoggedIn -> dataStore LoggedOut

// Logging in is a bit harder. Let's say that to log in to a data store, the
// user has to provide an username and a password. So, we need a function for
// validating passwords.
let validate (username password : string) : bool =
    if username = "K" && password = "MenInBlack"
    then true
    else false

// To log in, the user gives his password and username. Note that it is only
// possible to log in to a data store for which the user is logged out at first
// - this is guaranteed by the index on the dataStore argument. The result of
// the function depends on whether the password is valid: if it is, we get a
// data store which is in the LoggedIn state. Otherwise, we don't get anything
// useful and are left with the old data store, whose index is LoggedOut.
assume val login : (username : string)
                -> (password : string)
                -> dataStore LoggedOut
                -> (if validate username password
                    then dataStore LoggedIn
                    else unit)

// Let's also have a mock function for connecting to and disconnecting from
// data stores. We will require that upon connecting the user is logged out
// and also that he needs to log out before he can disconnect.
assume val connect    : string -> dataStore LoggedOut
assume val disconnect : dataStore LoggedOut -> unit

// We can retrieve unclassified data from the statistical office without
// logging in.
let getStats : data =
    let ds = connect "Statistical Office" in
    let stats = readPublicData ds in
    let _ = disconnect ds in
    stats
    
// To access top secret data from 3 letter agencies, we need to log in.
let getTopSecret : data =
    let ds = connect "FBI CIA etc." in
    let ds' = login "K" "MenInBlack" ds in
    let topSecret = readTopSecretData ds' in
    let ds'' = logout ds' in
    let _ = disconnect ds'' in
    topSecret

// But if we try to access secret data without logging in, we get a type error.
[@@ expect_failure]
let unauthorized : data =
    let ds = connect "FBI CIA etc." in
    let topSecret = readTopSecretData ds in
    let _ = disconnect ds in
    topSecret

// When should we use the above technique? It's particularly useful for
// designing libraries, APIs etc. which will then be used by some external
// users. The additional information they get in the indices makes it much
// harder for them to introduce bugs into their code. Moreover, it not only
// makes writing buggy code harder, it makes writing correct code easier -
// writing code can now be driven by looking at the types instead of looking
// at the documentation. This is called 'type-driven development' - check it
// out if you have some spare time.