module DependentRecords

open FStar.String

// 1. Ordinary pairs.

// An ordinary pair.
let p : int * string = (42, "42")

// Projections.
let n = fst p
let s = snd p

// We can pattern match on pairs.
let concat (p : int * string) : string =
    match p with
    | (n, s) -> string_of_int n ^ s

// 2. Dependent pairs.

// A dependent pair - the TYPE of the second component depends on the
// VALUE of the first component.
let p' : b : bool & (if b then nat else string) =
    (| false, "false" |)

// There are no built-in projections for dependent pairs, but we can
// define them using pattern matching.
let fst' (#a : Type) (#b : a -> Type) (p : (x : a & b x)) : a =
    match p with
    | (| x, y |) -> x

let snd' (#a : Type) (#b : a -> Type) (p : (x : a & b x)) : b (fst' p) =
    match p with
    | (| x, y |) -> y

// Defining projections for iterated dependent pairs is SO ANNOYING I won't
// even try to do it. It's much easier to use dependent records instead.



// 2. Dependent records.

// But what are dependent records good for?

// Forms! Everybody loves filling in forms, right? No? Well, dependent records
// can make forms a bit less awful.

// Surely you have at some point in your life encountered a question like
// "Why do you like pizza?". These questions have presuppositions: asking
// the question presupposes that certain facts are true. In our example, the
// presupposition is that you like pizza.

// Questions with presuppositions are annoying to model in forms, because it's
// not clear how to model the presuppositions. One common solution is to add
// an earlier question which asks about the presupposition and then provide
// an additional answer to the main question, like "Not Applicable".

type pizzaReason = | NotApplicable | ItsCheap | ItsTasty | Other

type pizzaForm =
{
    doYouLikePizza    : bool;
    whyDoYouLikePizza : pizzaReason;
}

// But this is a very weak solution, because now somebody can answer "Yes" to
// the presupposition question and "Not Applicable" to the main question, which
// was not intended. Conversely, somebody might answer "No" to the first
// question, but then answer the main question with something else than
// "Not Applicable", 

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

// Let's say you're programming forms for an institution which asks these kinds
// of questions. Now you need to implement a validator for the form in order
// to rule out the incorrect answer combinations.

let validPizzaForm (f : pizzaForm) : bool =
    match f.doYouLikePizza, f.whyDoYouLikePizza with
    | true, NotApplicable  -> false
    | true, _              -> true
    | false, NotApplicable -> true
    | false, _             -> false

// But the situation is still pretty bad, because internally you work with a
// representation which allows these incorrect answer combinations. In
// functional programming this is considered a bad practice - you should "make
// illegal states unrepresentable".

// Dependent records can help you here - you can use them to precisely describe,
// at the type level, which questions depend on which previous questions and
// precisely in what way.

// We don't need NotApplicable anymore, so we make a new type.
type pizzaReason' = | ItsCheap' | ItsTasty' | Other'

type dependentPizzaForm =
{
    doYouLikePizza'    : bool;
    
    // This is the essence of dependent records: the TYPE of the field
    // whyDoYouLikePizza' depends on the VALUE of the field doYouLikePizza'.
    // In case the value is true, the type is pizzaReason'. In case it's
    // false, the type is unit, i.e. the type that has only one element.
    whyDoYouLikePizza' : (if doYouLikePizza' then pizzaReason' else unit);
}

// People who like pizza must give a correct reason for it.
let like : dependentPizzaForm =
{
    doYouLikePizza'    = true;
    whyDoYouLikePizza' = ItsTasty';
}

// People who don't like pizza can't provide a reason for why they like it,
// but they need to enter (), i.e. the only value of the unit type, as
// the reason.
let dislike : dependentPizzaForm =
{
    doYouLikePizza'    = false;
    whyDoYouLikePizza' = ();
}

// Of course, it is also possible to model this simple form using ordinary
// algebraic data types, by making presuppositions into constructors and
// attaching the type of answers to the main question into the appropriate
// constructor.

type algebraicPizzaForm =
    | DoesntLikePizza : algebraicPizzaForm
    | LikesPizza      : (r : pizzaReason') -> algebraicPizzaForm

// But this works only because we're dealing with a single, simple question.
// If we wanted to model a question with more complicated presuppositions or
// more dependencies, the above solution would quickly degenerate into a mess.

// Dependent records can, just like ordinary records, be nested. Together with
// dependencies, this gives us a nice ability to include or exclude nested
// subforms based on answers to previous questions. Let's see a bigger example.

// We will ask about some personal data.
type nationality = | American | Polish

// SSN is the American Social Security Number and PESEL is a similar thing for
// Poland. We define them as nat/string just to show that the type of ID we
// ask for can depend on the nationality.
let ssn : Type = nat
let pesel : Type = string

// We will have a subforms that ask about covid and vaccines.
type covidStatus = | Healthy | Ill | Recovered | Dead

type covidSubform =
{
    wereYouHospitalized : bool;
    forHowManyDays :
        (match wereYouHospitalized with
         | true  -> nat
         | false -> unit);
}

type vaccineCompany = | Pfizer | Moderna | AstraZeneca

type vaccineSubform =
{
    willYouVaccinate : bool;
    
    whatVaccine :
        (match willYouVaccinate with
            | true -> vaccineCompany
            | false -> unit);
}

// We will also have a subform that asks about programming.
type progLang = | Haskell | Fsharp | Python | Cpp | OtherLang

type programmingSubform =
{
    isProgrammingYourDailyJob : bool;
    
    whatDoYouUseAtWork :
        normalize (match isProgrammingYourDailyJob with
         | true  -> progLang
         | false -> unit);

    doYouKnowHaskell :
        (match isProgrammingYourDailyJob with
            | true ->
                (match normalize whatDoYouUseAtWork with
                    | Haskell -> unit
                    | _       -> bool)
            | false -> bool);

    favouriteLang : progLang;
}

type bigForm =
{
    // Ordinary, non-dependent fields.
    firstName : string;
    
    // Beware: fields and types can have the same name!
    nationality : nationality;
    
    // What type of ID we ask for depends on nationality of the person.
    // To put it more explicitly: the TYPE of 'id' depends on the VALUE of
    // 'nationality'.
    id : (match nationality with
          | Polish   -> pesel
          | American -> ssn);

    // Here we have an example of a subform which is included or excluded
    // depending on the value of the field 'areYouAProgrammer'. We model
    // "excluding" a subform with the type unit, i.e. the type that has
    // only a single element.
    areYouAProgrammer : bool;
    programmingSubform :
        (match areYouAProgrammer with
        | true  -> programmingSubform
        | false -> unit);

    // Same story as above - we include the covid subform only for if the
    // value of 'covidStatus' is Ill or Recovered, and we exclude the
    // subform if the value is Healthy or Dead.
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