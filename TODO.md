The text below is what remains of the great plan to roll out a series of 5 lectures about F* at Credit Suisse. In the end, only one lecture was given that corresponds to roughly the first and the beginning of the second planned of the planned lectures.

# Lecture 1

Incorporated into the standalone lecture.

# Lecture 2

## Introduction

Partially incorporated into the standalone lecture.

## The rest of the introduction

### Falsity and negation
- Introduce the empty type using the mechanism of algebraic data types.
- Introduce the proposition False as the "propositions are types" interpretation of the empty type.
- Introduce negation as an implication whose codomain is False. Explain the meaning of negation ~ P as there being some internal inconsistency in P.
- BEWARE! Remember to discuss ex falso carefully as this aspect of negation is neither intuitive to ordinary people nor to programmers (the empty type is not very useful in F# or Haskell).
- Optional: mention strong negation.

## Higher-order logic: you already know it

### Predicates and relations

Explain the notions of predicate and relation and then introduce notation (i.e. A -> Type and A -> B -> Type).

### Quantifiers
- Describe rules for quantifiers by referring back to dependent function and pair types from last lecture. Mention that F* can prove some propositions all by itself, using only the built-in SMT solvers.
- BEWARE! Make sure to strongly emphasize that in this interpretation logic is constructive and explain what that means.

## Induction is recursion
- Start with a refresher on recursion. Examples: basic list functions -- append, reverse, etc.
- Then explain proofs by induction by analogy with recursive definitions. Examples: properties of basic list functions.

## Inductive predicates and relations

Explain how to interpret inductive families and predicates and relations. Examples: list permutations, being element of a list, there exists/all elements of a list satisfy some predicate, a list has duplicate elements, etc.

### Undecidability and generative thinking

Explain the difference between defining a function which checks if a property holds ("how to check" is a kind of top-down thinking) and defining a property as an inductive family ("how to generate all proofs of this" is a kind of bottom-up thinking).

### Proof relevance

Explain the notion of proof relevance, i.e. that there can be different proofs of some propositions, represented by different elements of the corresponding type. Example: being an element of a list.

## Equality

Some background on the classical definitions of equality by Aristotle and Leibniz.

### Definition and convertibility
- Show how to define equality as an inductive family.
- Show some example proofs, like 2 + 2 = 4 or some more properties of list functions.
- Explain why this definition even makes sense by explaining how computation works and the notion of convertibility.

### Properties of equality
- Prove basic properties of equality, like symmetry and transitivity.
- Also prove some characterizations of equality for some type formers, like pairs or sums.

### Caveat: equality of functions and types
- Explain why we can't prove that extensionally equal functions are equal.
- Explain why we can't prove that isomorphic types are equal. Some silly example: list and list', where list' is just a primed copy of list.

### Caveats: decidable and heterogenous equality

Reminder: differences between differents kinds of "equality": decidable equality, homogenous equality and heterogenous equality.

## Axioms and classical logic
- Explain how to declare axioms and that assuming an axiom breaks normalization (i.e programs built from axioms don't always compute).
- Example 1: functional extensionality axiom.
- Exmaple 2: excluded middle.

## How to find proofs

Because propositions are types and proofs are programs, finding proofs is basically the same as writing programs. All the techniques programmers already know, like splitting a function into subfunctions or refactoring some code into a separate functions have their exact equivalents (splitting a proof into lemmas and refactoring a part of a proofs into a lemma, respectively).

## Exercises

Some basic properties of connectives and quantifiers. Practice defining predicates and relations using inductive families. Implement some decision procedures for equality of numbers, prove some equational properties of numbers.

# Lecture 3

## The Big Lie: only some programs are proofs

Explain that during the last lecture we may have been led into believing some lies, namely that ALL programs are proofs, which is not true in general and not true neither for F# nor Haskell or any other mainstream functional language.

## Universes revisited

Explain Russell's paradox and then expose lie #1 that we have been pretending is true up to now (i.e. that Type : Type). Mention the universe hierarchy only briefly as a technical device to avoid the paradox, don't go into details.

## Why infinite loops are evil

Start with a pseudocode example showing how to prove a contradiction using a nonterminating program. Use this to motivate termination checking.

## Termination checking and well-foundedness

Introduce the notion of well-founded relations. This may be done in a simplified way in order not to bother people with definitions. Basically in this view well-founded = argument somehow decreases. Then discuss the most important ways of creating well-founded relations, i.e. products and images. Discuss at least how termination checking is done in F*.

## Positivity checking
- Give an example of a silly inductive type (along the lines of Bad = Bad -> Bad) which allows us to implement recursive programs without using recursion. Show how to use it to derive a contradiction.
- Then introduce the strict positivity criterion, i.e. inductive type I can't have argument that have an occurrence of I to the left of an arrow.

## Why throwing exceptions is evil

Show how to prove a contradiction by throwing an exception. Use this to motivate a deeper investigation into what stuff can programs do (besides number crunching of course) to be considered valid proofs.

## Purity and referential transparency
- Introduce (or reintroduce, depending on audience) the notions of purity and referential transparency in the way they are understood in Haskell folk wisdom.
- Silly examples: Edinburgh is the capital of Scotland, Mary Jane loves Peter Parker. Less silly examples: random() + random () != 2 * random()

## Effects in F*
- Introduce the mechanism of effects as a way of staticly checking what side effects programs are allowed to perform.
- Explain how the effect system can be used to reconcile proofs and programs so that programs can perform effects if allowed to, but proofs can't.
- An interesting example: show a proof of a contradiction inside an effect which allows nontermination and tell people that it's ok and safe.

## Exercises
- Some repetitive exercises for concept testing.
- Exercise 1: (why) is this function terminating?
- Exercise 2: is this inductive type strictly positive?
- Less repetitive exercises: no idea.

# Lecture 4

## Modules and interfaces
- Explain the F* module system by analogy with the F# module system.
- Explicitly mention that modules are dependently typed, so that later fields can depend on earlier fields.
- Explicitly mention that modules can contain fields which are lemmas/theorems/laws/specifications, so that we can force the implementer to provide a well-behaved implementation.
- Mention that modules cannot be nested.
- Digression: module lemmas as hints for F*'s SMT solver using SMTPattern.

## Longer examples

Maybe a full quicksort, similar to the one from my thesis?

## Exercises

No idea yet.

# Lecture 5

This lecture was to be about more technical F* stuff.