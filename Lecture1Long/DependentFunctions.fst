module DependentFunctions

// We will now explain how to use dependent functions.

// Let's start with a silly example.
let silly (b : bool) : (if b then string else int) =
    if b
    then "I FEEL LIKE I'M DYNAMICALLY TYPED, BUT NOPE!"
    else 42

// 'silly' is a function that takes b : bool as input and returns...
// Recall that in F* types are first-class, which means, among others, that
// we can compute them at runtime. (if b then string else int) is precisely
// such a thing - a type computed at runtime based on the value of b.

// What 'silly' does is check whether b is true or false and then returns
// some value of the appropriate type.

// Note that this looks similar to what can be done in dynamically typed
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
let s = printf "%d + %d = %d" 1 2 3
let s' = printf "not %b is %b" true false
let s'' = printf "I like trains!"

// Note that the format string needs not be known at compile time. It can be
// provided at runtime, but it will be pretty difficult to call printf with
// such a format string.
// Also note that a type-safe print prevents many obscure security attacks, like
// https://en.wikipedia.org/wiki/Uncontrolled_format_string