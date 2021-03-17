module DependentRecords

open FStar.String
open FStar.Squash

// An ordinary pair.
let p : int * string = (42, "42")

// Projections.
let n = fst p
let s = snd p

// We can pattern match on pairs.
let concat (p : int * string) : string =
    match p with
    | (n, s) -> string_of_int n ^ s

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
// even try. It's much easier to use dependent records instead.

// But what are dependent records good for?

// Forms! Everybody loves filling in forms, right? No? Well, dependent records
// can make forms a bit less awful.

// Surely you have at some point in your life encountered a form which had a
// question of the form "Did you stop beating your wife?" These questions are
// annoying, because they have presuppositions.

// These presuppositions are often posed earlier as separate questions: "Do you
// beat your wife?". Then, the main question will get an additional answer, like
// "Not Applicable".

// But this is a very weak solution, because now somebody can answer "Yes" to
// the presupposition question and "Not Applicable" to the main question, which
// was not intended and results in an incorrect state.

// Let's say you're programming forms for an institution which asks these kinds
// of questions. Now you need to implement some kind of form validator in order
// to rule out the incorrect answer combinations. But the situation is still
// pretty bad, because internally you work with a representation which allows
// these incorrect answer combinations.

// Functional programming considers this a bad practice - you should "make
// incorrect states unrepresentable". Dependent records can help you here - you
// can use them to precisely describe, at the type level, which questions
// depend on which previous questions and precisely in what way. This can be
// pushed even further - you can include or exclude nested subforms based on
// answers to previous questions.

// If this whole example sounds silly to you, well... this kind of thing is
// one of the most important reasons why we are CS are even trying to use F*
// in production.

// Enough talking, on to the (admittedly, silly) example.

type nationality = | Polish | American

// SSN is the American Social Security Number and PESEL is a similar thing for
// Poland. We define both as string because this is just an example.
let ssn : Type = string
let pesel : Type = string

type covidStatus = | Healthy | Ill | Recovered

// A subform that asks about covid.
type covidSubform =
{
    wereYouHospitalized : bool;
    forHowManyDays :
        (match wereYouHospitalized with
         | true  -> nat
         | false -> unit);
    
    willYouVaccinate : bool;
}

type progLang = | Haskell | Fsharp | Python | Cpp | Other

// A subform that asks about programming.
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

type eyeColor = | Blue | Green | Brown

type bigForm =
{
    // Ordinary, non-dependent fields.
    firstName : string;
    eyeColor : eyeColor;
    
    // Weird: fields and types can have the same name!
    nationality : nationality;
    
    // What type of ID we ask for depends on nationality of the person.
    id : (match nationality with
          | Polish   -> pesel
          | American -> ssn);

    doYouHaveAWife : bool;
    
    // The TYPE of 'doYouBeatYourWife' depends on the VALUE of the field
    // 'doYouHaveAWife'. In case the latter is true, the possible answers
    // are of type bool (i.e. true or false). But in case 'doYouHaveAWife'
    // is false, the type of 'doYouBeatYourWife' is unit - you can't beat
    // your wife if you don't have one, and there's no point in even
    // asking such a question!
    doYouBeatYourWife :
        (match doYouHaveAWife with
         | true -> bool
         | false -> unit);

    // Similar story as above, but this time if somebody says he's a
    // programmer, we ask him a few programming questions. Otherwise,
    // we skip the whole subform.
    areYouAProgrammer : bool;
    programmingSubform :
        (match areYouAProgrammer with
        | true  -> programmingSubform
        | false -> unit);

    // Once again the same thing, but for covid.
    covidStatus : covidStatus;
    covidSubform :
        (match covidStatus with
         | Ill | Recovered -> covidSubform
         | _               -> unit);
}

let me : bigForm =
{
    firstName = "Wojciech";
    eyeColor  = Brown;

    nationality = Polish;
    id = "Not gonna disclose this";

    doYouHaveAWife = false;
    doYouBeatYourWife = ();

    areYouAProgrammer = true;
    programmingSubform =
    {
        isProgrammingYourDailyJob = true;
        whatDoYouUseAtWork = Fsharp;
        doYouKnowHaskell = true;
        favouriteLang  = Other;
    };

    covidStatus = Healthy;
    covidSubform = ();
}