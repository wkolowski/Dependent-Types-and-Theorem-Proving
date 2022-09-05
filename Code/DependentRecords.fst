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
let p1 : (x : bool & (if x then int else string)) =
    (| false, "false" |)

let p2 : (x : bool & (if x then int else string)) =
    (| true, -1234567890 |)

// Note that this looks similar to what can be done in dynamically typed
// languages like Python, but here the typing is static - let's try to change
// the value of the second component of the pair and see what happens.

// This is not well-typed, because for b = false the second component should
// be a string, not an int.
// an int was expected.
[@@ expect_failure]
let p3 : (x : bool & (if x then int else string)) =
    (| false, 123 |)

// Similarly, this is not well-typed because for b = true we return a string,
// but an int was expected.
[@@ expect_failure]
let p4 : (x : bool & (if x then int else string))  =
    (| true, "hello there" |)

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
type pizzaReason2 = | ItsCheap2 | ItsTasty2 | Other2

type dependentPizzaForm =
{
    doYouLikePizza2    : bool;
    
    // This is the essence of dependent records: the TYPE of the field
    // whyDoYouLikePizza2 depends on the VALUE of the field doYouLikePizza2.
    // In case the value is true, the type is pizzaReason2. In case it's
    // false, the type is unit, i.e. the type that has only one element.
    whyDoYouLikePizza2 : (if doYouLikePizza2 then pizzaReason2 else unit);
}

// People who like pizza must give a correct reason for it.
let like : dependentPizzaForm =
{
    doYouLikePizza2    = true;
    whyDoYouLikePizza2 = ItsTasty2;
}

// People who don't like pizza can't provide a reason for why they like it,
// but they need to enter (), i.e. the only value of the unit type, as
// the reason.
let dislike : dependentPizzaForm =
{
    doYouLikePizza2    = false;
    whyDoYouLikePizza2 = ();
}

// Of course, it is also possible to model this simple form using ordinary
// algebraic data types, by making presuppositions into constructors and
// attaching the type of answers to the main question into the appropriate
// constructor.

type algebraicPizzaForm =
    | DoesntLikePizza : algebraicPizzaForm
    | LikesPizza      : (r : pizzaReason2) -> algebraicPizzaForm

// But this works only because we're dealing with a single, simple question.
// If we wanted to model a question with more complicated presuppositions or
// more dependencies, the above solution would quickly degenerate into a mess.

// Dependent records can, just like ordinary records, be nested. Together with
// dependencies, this gives us a nice ability to include or exclude nested
// subforms based on answers to previous questions. Let's see a bigger example.

// We will ask about some personal data.
type nationality = | American | Polish

// SSN is the American Social Security Number and PESEL is a similar thing for
// Poland. We define them as int/string just to show that the type of ID we
// ask for can depend on the nationality.
let ssn : Type = int
let pesel : Type = string

// We will have two subforms that ask about covid hospitalization and vaccines,
// depending on the person's current covid status.
type covidStatus = | Healthy | Ill | Recovered | Dead

type hospitalizationSubform =
{
    wereYouHospitalized : bool;
    forHowManyDays :
        (match wereYouHospitalized with
         | true  -> nat
         | false -> unit);
}

type vaccineCompany = | Pfizer | Moderna | AstraZeneca | Whatever

type vaccineSubform =
{
    willYouVaccinate : bool;
    
    whatVaccine :
        (match willYouVaccinate with
            | true -> vaccineCompany
            | false -> unit);
}

// We will also have a subform that asks about programming and particularly
// whether the person knows Haskell.
type progLang = | Haskell | Fsharp | Python | Cpp | OtherLang

type programmingSubform =
{
    isProgrammingYourDailyJob : bool;
    
    whatDoYouUseAtWork :
        (match isProgrammingYourDailyJob with
         | true  -> progLang
         | false -> unit);

    favouriteLang : progLang;

    doYouKnowHaskell :
        (match favouriteLang with
            | Haskell -> unit
            | _       -> bool);
        (*(match isProgrammingYourDailyJob with
            | true ->
                normalize (match whatDoYouUseAtWork with
                    | Haskell -> unit
                    | _       -> bool)
            | false -> bool);*)
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
         | Healthy         -> vaccineSubform
         | Ill | Recovered -> hospitalizationSubform
         | Dead            -> unit);
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
        favouriteLang  = OtherLang;
        doYouKnowHaskell = true;
    };

    covidStatus = Healthy;
    covidSubform =
    {
        willYouVaccinate = true;
        whatVaccine = Whatever;
    };
}

let not_me : bigForm =
{
    firstName = "Jonathan";

    nationality = American;
    id = 124567890;

    areYouAProgrammer = false;
    programmingSubform = ();

    covidStatus = Ill;
    covidSubform =
    {
        wereYouHospitalized = true;
        forHowManyDays = 360;
    };
}

[@@ expect_failure]
let bad_fill : bigForm =
{
    firstName = "asdf";

    nationality = American;

    // Wrong type, for Americans this should be an int!
    id = "lalalala";

    areYouAProgrammer = false;

    // Wrong type - non-programmers can't answer these questions!
    programmingSubform =
    {
        isProgrammingYourDailyJob = true;
        whatDoYouUseAtWork = Fsharp;
        favouriteLang  = Haskell;

        // Not possible not to know Haskell when it's your favourite lang.
        doYouKnowHaskell = false;
    };

    covidStatus = Healthy;

    // Wrong subform!
    covidSubform =
    {
        wereYouHospitalized = true;
        forHowManyDays = 100;
    };
}