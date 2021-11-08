module DependentFunctions




let silly (b : bool) : (if b then string else int) =
    if b
    then "I FEEL LIKE I'M DYNAMICALLY TYPED, BUT NOPE!"
    else 42


















let identity : (a : Type) -> a -> a =
    fun (a : Type) (x : a) -> x









let idstring : string -> string = identity string
let idint : int -> int = identity int





let identity' (a : Type) (x : a) : a = x




let five = identity' int 5



let identity'' (#a : Type) (x : a) : a = x


let five' = identity'' 5








open FStar.Char
open FStar.String










let rec typeOfPrintf (format : list char) : Type =
    match format with
    | []                    ->         string
    | '%' :: 'd' :: format' -> int  -> typeOfPrintf format'
    | '%' :: 'b' :: format' -> bool -> typeOfPrintf format'
    | _          :: format' ->         typeOfPrintf format'
















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
















let printf (format : string) : typeOfPrintf (list_of_string format) =
    printf_aux (list_of_string format) ""
    









let s = printf "%d + %d = %d" 1 2 3
let s' = printf "not %b is %b" true false
let s'' = printf "I like trains!"












































type loginState =
    | LoggedIn
    | LoggedOut




assume type dataStore : loginState -> Type




assume type data



assume val readPublicData    : (#s : loginState) -> dataStore s -> data




assume val readTopSecretData : dataStore LoggedIn -> data





assume val logout : dataStore LoggedIn -> dataStore LoggedOut




let validate (username password : string) : bool =
    if username = "K" && password = "MenInBlack"
    then true
    else false







assume val login : (username : string)
                -> (password : string)
                -> dataStore LoggedOut
                -> (if validate username password
                    then dataStore LoggedIn
                    else unit)




assume val connect    : string -> dataStore LoggedOut
assume val disconnect : dataStore LoggedOut -> unit



let getStats : data =
    let ds = connect "Statistical Office" in
    let stats = readPublicData ds in
    let _ = disconnect ds in
    stats
    

let getTopSecret : data =
    let ds = connect "FBI CIA etc." in
    let ds' = login "K" "MenInBlack" ds in
    let topSecret = readTopSecretData ds' in
    let ds'' = logout ds' in
    let _ = disconnect ds'' in
    topSecret


[@@ expect_failure]
let unauthorized : data =
    let ds = connect "FBI CIA etc." in
    let topSecret = readTopSecretData ds in
    let _ = disconnect ds in
    topSecret








