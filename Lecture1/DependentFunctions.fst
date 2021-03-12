module DependentFunctions

// We will now explain how to use dependent functions.

// Let's start with a silly example.
let silly (b : bool) : (if b then string else int) =
    if b
    then "I FEEL LIKE I'M DYNAMICALLY TYPED, BUT NOPE!"
    else 42

// 'silly' is a function that takes b : bool as input and returns...
// Recall that in F* types are first-class, which means, among others, that
// we can compute them at runtime.

// (if b then string else int) is precisely such a thing - a type computed
// at runtime based on the value of b.

// What 'silly' does is check whether b is true or false and then returns
// some value of the appropriate type.

// Note the this looks similar to what can be done in dynamically typed
// languages like Python, but here the typing is static - try to change
// the value returned in the 'then' branch of 'silly' and see what F* tells you.



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
// Below, string -> string clearly isn't the same thing as int -> int.
let idstring : string -> string = identity string
let idint : int -> int = identity int

// Note that the definition of 'identity' was a bit verbose. We can make it
// shorter by moving all arguments to the left of the semicolon.
// This results in a situation, in which the types of later arguments (x : a)
// can depend on the types of earlier arguments (a : Type).
let identity' (a : Type) (x : a) : a = x

// But then, we may notice some redundancy when calling such functions.
// For example, if we call identity' with x = 5, then we can infer a = int,
// because 5 : int.
let five = identity' int 5

// F* can infer these arguments too, provided we mark them as implicit,
// which we can do by putting a hash # in front of that argument's name.
let identity'' (#a : Type) (x : a) : a = x

// We can call identity'' with 5 only and F* infers a = int.
let five' = identity'' 5



// A typical implementation of function composition looks like this.
let comp (#a #b #c : Type) (f : a -> b) (g : b -> c) (x : a) : c =
    g (f x)



//\subsection{Example: typesafe printf}
//% Finish the typesafe printf example.