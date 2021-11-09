module DependentFunctions

// We will now explain how to use dependent functions.

// Let's start with a silly example.
let silly (x : bool) : (if x then string else int) =
    if x
    then "I FEEL LIKE I'M DYNAMICALLY TYPED, BUT NOPE!"
    else 42

// 'silly' is a function that takes b : bool as input and returns...
// Recall that in F* types are first-class, which means, among others, that
// we can compute them at runtime. (if b then string else int) is precisely
// such a thing - a type computed at runtime based on the value of b.

// What 'silly' does is check whether b is true or false and then returns
// some value of the appropriate type.

// Note that this looks similar to what can be done in dynamically typed
// languages like Python, but here the typing is static - let's try to change
// the value returned by 'silly' and see what F* tells us.

// This is not well-typed, because for b = false we return a string, whereas
// an int was expected.
[@@ expect_failure]
let silly2 (x : bool) : (if x then string else int) =
    "Always a string"

// Similarly, this is not well-typed because for b = true we return an int,
// but a string was expected.
[@@ expect_failure]
let silly3 (x : bool) : (if x then string else int) =
    1234567890

// This one is not well-typed too. For b = true we return an int, but a string
// was expected. For b = false, we return a string, but an int was expected.
[@@ expect_failure]
let silly2 (x : bool) : (if x then string else int) =
    if x
    then 15
    else "a string"



// In F*, dependent functions are ubiquitous. They are used for everything,
// including polymorphism. To make a polymorphic function, we just need to
// take a Type as input.
let identity : (a : Type) -> a -> a =
    fun (a : Type) (x : a) -> x

// 'identity' is a function that takes a type named a as input and returns a
// function of type a -> a.

// Note that it is a dependent function, because the output TYPE (a -> a)
// depends on the input VALUE (a : Type).

// This means that for different inputs, the outputs are of different types.
// Below, string -> string clearly isn't the same type as int -> int.
let idstring : string -> string = identity string
let idint : int -> int = identity int

// Note that the definition of 'identity' was a bit verbose. We can make it
// shorter by moving all arguments to the left of the semicolon.
// This results in a situation, in which the types of later arguments (x : a)
// can depend on the types of earlier arguments (a : Type).
let identity' (a : Type) (x : a) : a = x

// But then, we may notice some redundancy when calling such functions.
// For example, if we call identity' with x = 5, then we can infer that
// a must be int, because 5 : int.
let five = identity' int 5

// F* can infer these arguments too, provided we mark them as implicit,
// which we can do by putting a # in front of that argument's name.
let identity'' (#a : Type) (x : a) : a = x

// We can call identity'' with 5 only and F* infers a = int.
let five' = identity'' 5



// We're now ready to see a serious example of a dependent function. - we will
// develop a typesafe printf function.

// We can use the 'open' keyword to import a module. We will need these
// for string manipulation.
open FStar.Char
open FStar.String

// printf is a natural example of a dependent function: what arguments it takes
// (and how many of them) depends on the format string. We can thus compute
// its precise type by recursion on the format string and then use dependent
// function types to make sure arguments provided by the caller are of the
// right types.

// To make our lives easier, we will only consider ints (with format "%d")
// and booleans (with format "%b"). We will work with lists of chars and will
// deal with strings later.
let rec typeOfPrintf (format : list char) : Type =
    match format with
    | []                    ->         string
    | '%' :: 'd' :: format' -> int  -> typeOfPrintf format'
    | '%' :: 'b' :: format' -> bool -> typeOfPrintf format'
    | _          :: format' ->         typeOfPrintf format'

// typeOfPrintf takes a format string (well, list of chars) as input and
// returns a Type.
// The function works as follows:
// - if the format string is empty, we return the type 'string', because
//   the output of sprintf will be a string
// - if the format string starts with "%d", then sprintf will need to take
//   an additional int arguemnt
// - if the format string starts with "%b", then sprintf will need to take
//   an additional bool arguemnt
// - if the format string starts with some other character, we just
//   recurse on the rest of the format string

// We won't actually print anything to the screen, just convert stuff to string.
// You can ignore the 'normalize' that appears in the type - it's a quirk of
// F* that forces the expression 'typeOfPrint format' to be fully evaluated.
let rec printf_aux (format : list char) (acc : string)
    : normalize (typeOfPrintf format) =
    match format with
    | []                    -> acc
    | '%' :: 'd' :: format' ->
        fun (i : int) -> printf_aux format' (acc ^ string_of_int i)
    | '%' :: 'b' :: format' ->
        fun (b : bool) -> printf_aux format' (acc ^ string_of_bool b)
    | c          :: format' ->
        printf_aux format' (acc ^ string_of_char c)

// printf_aux takes as input a format string and an accumulator, which
// represents the portion of format string processed so far.
// The function works as follows:
// - If the format string is empty, we're done and we can return the
//   processed string (i.e. the accumulator acc).
// - If the format string starts with "%d", we need to take an additional
//   argument which is an int. We recurse on the remaining part of the 
//   format string with the integer appended to the accumulator.
// - If the format string starts with "%b", we proceed as above, but with
//   a boolean instead of an integer/
// - If the format string starts with any other character, we recurse
//   on the remaining part of the format string with the character
//   appended to the accumulator.

// Voilà! Here is the final version of our type-safe printf.
let printf (format : string) : typeOfPrintf (list_of_string format) =
    printf_aux (list_of_string format) ""
    
// As input we take the format string (which, this time, is really a string and
// not a list of chars) and we return an element of the appropriate type.
// Note that this time we don't need the quirky 'normalize', but we need to
// convert the format string into a list of chars in the return type.
// The implementation is simple: we call our auxiliary function with
// the format string converted to a list of chars and an empty accumulator.

// We can call printf only with the right number of arguments of the correct
// type. Anything else will end with a type error.
let s1 : string = printf "%d + %d = %d" 1 2 3
let s2 : string = printf "not %b is %b" true false
let s3 : string = printf "I like trains!"

// If the arguments we give don't match what is expected from the format string,
// we get a type error.

// Not enough arguments - 3 ints were expected, 2 ints were given.
[@@ expect_failure]
let s4 : string = printf "%d + %d = %d" 1 2

// Last argument of the wrong type - should be bool instead of int.
[@@ expect_failure]
let s5 : string = printf "not %b is %b" true 42

// Too many arguments.
[@@ expect_failure]
let s6 : string = printf "I like trains!" 1234567890


// Note that the format string needs not be known at compile time. It can be
// provided at runtime, but it will be pretty difficult to call printf with
// such a format string.
// Also note that a type-safe print prevents many obscure security attacks, like
// https://en.wikipedia.org/wiki/Uncontrolled_format_string



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
    username = "K" && password = "MenInBlack"

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

(*
    The error message we get is this:
    
    (Error 189) Expected expression of type
    "DependentFunctions.dataStore (DependentFunctions.LoggedIn)";
    got expression "ds" of type
    "DependentFunctions.dataStore (DependentFunctions.LoggedOut)"
*)



// When should we use the above technique? It's particularly useful for
// designing libraries, APIs etc. which will then be used by some external
// users. The additional information they get in the indices makes it much
// harder for them to introduce bugs into their code. Moreover, it not only
// makes writing buggy code harder, it makes writing correct code easier -
// writing code can now be driven by looking at the types instead of looking
// at the documentation. This is called 'type-driven development' - check it
// out if you have some spare time.
