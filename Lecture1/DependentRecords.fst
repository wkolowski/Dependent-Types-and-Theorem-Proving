module DependentRecords

open FStar.String
//open FStar.Squash

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
// even try to do it. It's much easier to use dependent records instead.



// But what are dependent records good for?

// Forms! Everybody loves filling in forms, right? No? Well, dependent records
// can make forms a bit less awful.

// Surely you have at some point in your life encountered a question like
// "Did you stop smoking?". These are questions with presuppositions: asking
// the question presupposes that certain facts are true - in our example, the
// presupposition is that you were smoking.

// Such questions are very annoying to model in forms. The presuppositions are
// often posed earlier as separate questions: "Did you smoke?". Then, the main
// question will get an additional answer, like "Not Applicable".

// But this is a very weak solution, because now somebody can answer "Yes" to
// the presupposition question and "Not Applicable" to the main question, which
// was not intended and results in an incorrect state.

// Let's say you're programming forms for an institution which asks these kinds
// of questions. Now you need to implement a validator for the form in order
// to rule out the incorrect answer combinations. But the situation is still
// pretty bad, because internally you work with a representation which allows
// these incorrect answer combinations.

// Functional programming considers this a bad practice - you should "make
// illegal states unrepresentable". Dependent records can help you here - you
// can use them to precisely describe, at the type level, which questions
// depend on which previous questions and precisely in what way. This can be
// pushed even further - you can include or exclude nested subforms based on
// answers to previous questions.

// Enough talking, on to the (admittedly, silly) example.

// We will have a subform that asks about covid.
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

// We will also have a subform that asks about programming.
type progLang = | Haskell | Fsharp | Python | Cpp | Other

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

// And we will ask about some personal data.
type nationality = | Polish | American

// SSN is the American Social Security Number and PESEL is a similar thing for
// Poland. We define both as string because this is just an example.
let ssn : Type = string
let pesel : Type = string

// This is how dependent records work.
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
        favouriteLang  = Other;
    };

    covidStatus = Healthy;
    covidSubform = ();
}



// Another nice use of dependent records (well, of records in general too)
// is simulating Haskell's typeclasses, although without instance search.