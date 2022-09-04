module DependentRecords

open FStar.String

// Usual pairs.
let p : int * string = (42, "42")

let n = fst p
let s = snd p

let concat (p : int * string) : string =
    match p with
    | (n, s) -> string_of_int n ^ s



// Dependent pairs.
let p' : x : bool & (if x then nat else string) =
    (| false, "false" |)

let fst' (#a : Type) (#b : a -> Type) (p : (x : a & b x)) : a =
    match p with
    | (| x, y |) -> x

let snd' (#a : Type) (#b : a -> Type) (p : (x : a & b x)) : b (fst' p) =
    match p with
    | (| x, y |) -> y



// DO NOT REPRESENT FORMS LIKE THIS!
type pizzaReason = | NotApplicable | ItsCheap | ItsTasty | Other

type pizzaForm =
{
    doYouLikePizza    : bool;
    whyDoYouLikePizza : pizzaReason;
}

let evilAnswer : pizzaForm =
{
    doYouLikePizza = true;
    whyDoYouLikePizza = NotApplicable;
}

let evilAnswer2 : pizzaForm =
{
    doYouLikePizza = false;
    whyDoYouLikePizza = ItsTasty;
}

let validPizzaForm (f : pizzaForm) : bool =
    match f.doYouLikePizza, f.whyDoYouLikePizza with
    | true, NotApplicable  -> false
    | true, _              -> true
    | false, NotApplicable -> true
    | false, _             -> false



// This is how you should represent forms using dependent records.
type pizzaReason2 = | ItsCheap2 | ItsTasty2 | Other2

type dependentPizzaForm =
{
    doYouLikePizza2    : bool;
    whydoYouLikePizza2 : (if doYouLikePizza2 then pizzaReason2 else unit);
}

let like : dependentPizzaForm =
{
    doYouLikePizza2    = true;
    whydoYouLikePizza2 = ItsTasty2;
}

let dislike : dependentPizzaForm =
{
    doYouLikePizza2    = false;
    whydoYouLikePizza2 = ();
}

// Of course, such simple forms do not need dependent records.
type algebraicPizzaForm =
    | DoesntLikePizza : algebraicPizzaForm
    | LikesPizza      : (r : pizzaReason2) -> algebraicPizzaForm



// But more complex forms DO benefit from dependent records.
type covidStatus = | Healthy | Ill | Recovered | Dead

type covidSubform =
{
    wereYouHospitalized : bool;
    forHowManyDays :
        (match wereYouHospitalized with
         | true  -> nat
         | false -> unit);
    
    willYouVaccinate : bool;
}

type progLang = | Haskell | Fsharp | Python | Cpp | OtherLang

type programmingSubform =
{
    isProgrammingYourDailyJob : bool;
    whatDoYouUseAtWork :
        (match isProgrammingYourDailyJob with
         | true  -> progLang
         | false -> unit);

    doYouKnowHaskell : bool;
    favouriteLang : progLang;
}

type nationality = | Polish | American

let ssn : Type = int
let pesel : Type = string

type bigForm =
{
    
    firstName : string;
    
    nationality : nationality;
    id : (match nationality with
          | Polish   -> pesel
          | American -> ssn);

    areYouAProgrammer : bool;
    programmingSubform :
        (match areYouAProgrammer with
        | true  -> programmingSubform
        | false -> unit);

    covidStatus : covidStatus;
    covidSubform :
        (match covidStatus with
         | Ill | Recovered -> covidSubform
         | _               -> unit);
}

let me : bigForm =
{
    firstName = "Wojciech";

    nationality = Polish;
    id = "Not gonna disclose this";

    areYouAProgrammer = true;
    programmingSubform =
    {
        isProgrammingYourDailyJob = true;
        whatDoYouUseAtWork = Fsharp;
        doYouKnowHaskell = true;
        favouriteLang  = OtherLang;
    };

    covidStatus = Healthy;
    covidSubform = ();
}

let not_me : bigForm =
{
    firstName = "Jonathan";

    nationality = American;
    id = "Not gonna disclose this either";

    areYouAProgrammer = false;
    programmingSubform = ();

    covidStatus = Ill;
    covidSubform =
    {
        wereYouHospitalized = true;
        forHowManyDays = 60;
        willYouVaccinate = false;
    };
}