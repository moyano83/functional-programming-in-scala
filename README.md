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
we can imagine that computation proceeds much like we'd solve an algebraic equation, which enables equational reasoning about programs.

## Chapter 2: Getting started with functional programming in Scala<a name="Chapter2"></a>

### Introducing Scala the language: an example

In a method, the part of a declaration that goes before the equals sign as the left-hand side or signature, and the code that comes after the equals
sign as the right-hand side or definition. If T=the body of the method contains more than one statement, you need to put them inside curly braces. A
pair of braces containing statements is called a block.
A main method is an outer shell that calls into other code. We sometimes call such methods procedures or impure functions rather than functions, to
emphasize the fact that they have side effects. _Unit_ is a special type that is the return type of methods that doesn't return anything. There's
only one value of this type and the literal syntax for it is `()`.

### Running our program

The simplest way we can run a Scala program is from the command line, by invoking the Scala compiler directly: `scalac MyProgram.scala`. Then the
result can be run with `scala MyProgram` or it can also run in the REPL (which starts when we execute `scala`) like `scala > :load MyProgram.scala` as
long as the source file doesn't contain package declarations.

### Modules, objects, and namespaces

If we execute something like `MyProgram.someMethod(x)`, we say that MyProgram is the namespace of _someMethod_. Aside from some technicalities, every
value in Scala is what's called an _object_, and each object may have zero or more _members_. An object whose primary purpose is giving its members a
namespace is sometimes called a _module_. A member can be a method declared with the _def_ keyword, or it can be another object declared with _val_ or
object.

### Higher-order functions: passing functions to functions

_functions_ are values, and as such, they can be assigned to variables. A function that accepts other functions as arguments is called a
_higher-order function_. The way we write loops functionally, without mutating a loop variable, is with recursive functions (which parameter
values represent the state of the loop). A call is said to be in tail position if the caller does nothing other than return the value of the recursive
call which the scala compiler optimizes to the same sort of bytecode than for a while loop.

### Polymorphic functions: abstracting over types

Often, and especially when writing _higher order functions (HOF)_, we want to write code that works for any type it's given. These are called
polymorphic functions, as opposed to monomorphic functions which accepts only a type of parameter. An example of this would
be `def findFirst[A](as: Array[A], p: A => Boolean): Int`, and we can pass an anonymous function to this method like this `findFirst(Array(7, 9,
13), (x: Int) => x == 9)`. When we define a function literal, what is actually being defined in Scala is an object with a method called `apply`.
Scala has a special rule for this method name, so that objects that have an `apply` method can be called as if they were themselves methods.

### Following types to implementations

A function signature that can only be implemented in one way it's a higher-order function for performing what's called partial application. For
example a function that takes a value and a function of two arguments, and returns a function of one argument as its result. This is such a common
thing to want to do that Scala's standard library provides `compose` as a method on Function1 (the interface for functions that take one argument). To
compose two functions f and g, we simply say f compose g. It also provides an `andThen` method. `f andThen g` is the same as `g compose f`.

## Chapter 3: Functional Data Structures<a name="Chapter3"></a>
