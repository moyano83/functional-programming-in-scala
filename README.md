# Functional Programming in Scala

## Table of contents:

1. [Chapter 1: What is functional programming?](#Chapter1)
2. [Chapter 2: Getting started with functional programming in Scala](#Chapter2)
3. [Chapter 3: Functional data structures](#Chapter3)
4. [Chapter 4: Handling errors without exceptions](#Chapter4)

## Chapter 1: What is functional programming?<a name="Chapter1"></a>

Functional programming is reasoning in tearms of pure functions, that is, functions that does not have side effects. A formal definition of a pure
function is "a function _f_ with input type _A_ and output type _B_ (written in Scala as a single type: `A => B`, pronounced “A to B” or “A arrow B”)
is a computation that relates every value a of type _A_ to exactly one value b of type _B_ such that b is determined solely by the value of a. Any
changing state of an internal or external process is irrelevant to computing the result _f(a)_.
We can formalize this idea of pure functions using the concept of referential transparency (RT). This is a property of expressions in general and not
just functions, consider an expression to be any part of a program that can be evaluated to a result. RT states that in any program, a expression
can be replaced by its result without changing the meaning of the program.

Referential transparency forces the invariant that everything a function does is represented by the value that it returns, according to the result
type of the function. This constraint enables a simple and natural mode of reasoning about program evaluation called the _substitution model_. With RT
we can imagine that computation proceeds much like we’d solve an algebraic equation, which enables equational reasoning about programs.

## Chapter 2: Getting started with functional programming in Scala<a name="Chapter2"></a>

