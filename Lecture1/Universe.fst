module Universe

// 1. The criteria for being first-class.

// We will explain how first-class types work in F*. To do this, we will show
// that F* types satisfy all our first-class-ness criteria.

// In F*, just as in F#, we can create a type alias (also called type synonym or
// type abbreviation). We can do this with the keyword 'type'.
type integer = int

// An alias is just a new name for an old type.
let add_integer (n m : integer) : integer = n + m

// But in F* types are first-class, so we can do the same using the plain old
// 'let'. In other words, we can bind types to variables.
let integer' = int

let add_integer' (n m : integer') : integer = n + m

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



// 2. Defining type families by recursion.

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

// We can use vec to define a list of length 4.
let example_vec : vec nat 4 =
    (0, (1, (2, (3, ()))))

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
(*
let unauthorized : data =
    let ds = connect "FBI CIA etc." in
    let topSecret = readTopSecretData ds in
    let _ = disconnect ds in
    topSecret
*)

// When should we use the above technique? It's particularly useful for
// designing libraries, APIs etc. which will then be used by some external
// users. The additional information they get in the indices makes it much
// harder for them to introduce bugs into their code. Moreover, it not only
// makes writing buggy code harder, it makes writing correct code easier -
// writing code can now be driven by looking at the types instead of looking
// at the documentation. This is called 'type-driven development' - check it
// out if you have some spare time.