# Functional Programming in Scala

## Table of contents:

1. [Chapter 1: What is functional programming?](#Chapter1)
2. [Chapter 2: Getting started with functional programming in Scala](#Chapter2)
3. [Chapter 3: Functional data structures](#Chapter3)
4. [Chapter 4: Handling errors without exceptions](#Chapter4)
5. [Chapter 5: Strictness and laziness](#Chapter5)
6. [Chapter 6: Purely functional state](#Chapter6)

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

### Defining functional data structures

A functional data structure is operated on using only pure functions, functional data structures are by definition immutable.

### Pattern Matching

Scala has something similar to the _switch_ in java, which is pattern matching, although if any of the cases match in scala, the result of the
expression is the result of that particular case (the first one it matches). What determines if a pattern matches an expression? A pattern matches the
target if there exists an assignment of variables in the pattern to subexpressions of the target that make it structurally equivalent to the target.

### Data sharing in functional data structures

If a data structure such as List is immutable in scala, we can use a list to construct the resulting list of adding an element to that original list.
This is called data sharing. We say that functional data structures are persistent, meaning that existing references are never changed by operations
on the data structure. Writing purely functional data structures that support different operations efficiently is all about finding clever ways
to exploit data sharing. When a function definition contains multiple argument groups, type information flows from left to right across these argument
groups.

### Trees

An ADT is just a data type defined by one or more data constructors, each of which may contain zero or more arguments (_List_ is an example). We do
typically use ADTs for situations where the set of cases is closed or known to be fixed (For example a Tree structure has either a Leaf or a Branch).

## Chapter 4: Handling Errors without exceptions<a name="Chapter4"></a>

We can represent failures and exceptions with ordinary values, and we can write higher-order functions that abstract out common patterns of error
handling and recovery.

### The good and bad aspects of exceptions

RT expression may be substituted with the value it refers to, and this substitution should preserve program meaning. RT expressions does not depend on
context and may be reasoned about locally, whereas the meaning of non-RT expressions is context-dependent and requires more global reasoning. If an
exception is thrown, then execution of the program depends on where this exception is thrown.

    * Exceptions break RT and introduce context dependence, moving us away from the simple reasoning of the substitution model. Exceptions should 
      be used only for error handling, not for control flow.
    * Exceptions are not type-safe

Exceptions allow us to consolidate and centralize error-handling logic, but instead of throwing an exception, we return a value indicating that an
exceptional condition has occurred.

### Possible alternatives to exceptions

Returning an error code or something such as `Double.NaN` if an arithmetic operation is not defined is not recommended due to:

    * It allows errors to silently propagate. The caller can forget to check this condition and won't be alerted by the compiler
    * Results in a fair amount of boilerplate code at call sites, with explicit if statements to check whether the caller has received a valid result 
    * It's not applicable to polymorphic code. For some output types, we might not have a sentinel value of that type even if we wanted to.
    * It demands a special policy or calling convention of callers

We need a way to defer the decision of how to handle undefined cases so that they can be dealt with at the most appropriate level.

### The Option data type

The solution is to represent explicitly in the return type that a function may not always have an answer. `Option` deals with this, it takes each
value of the input type to exactly one value of the output type. Option is convenient as we can factor out common patterns of error handling
via higher-order functions, freeing us from writing the usual boilerplate that comes with exception-handling code.

A common pattern is to transform an Option via calls to `map`, `flatMap`, and/or `filter`, and then use `getOrElse` to do error handling at the end.
`orElse` is similar to `getOrElse`, except that we return another Option if the first is undefined.

We can `lift` ordinary functions to become functions that operate on Option. Any function that we already have lying around can be transformed
(via `lift`) to operate within the context of a single Option value. Since lifting functions is so common in Scala, Scala provides a syntactic
construct called the _for-comprehension_ that it expands automatically to a series of `flatMap` and `map` calls.

### The Either data type

One thing you may have noticed with Option is that it doesn't tell us anything about what went wrong in the case of an exceptional condition. The
Either data type represents, in a very general way, values that can be one of two things, it's a disjoint union of two types. When we use it to
indicate success or failure, by convention the Right constructor is reserved for the success case.

## Chapter 5: Strictness and laziness<a name="Chapter5"></a>

Composing programs using higher-order functions like `map` and `filter` instead of writing monolithic loops is possible through the use of
non-strictness (laziness).

### Strict and non-strict functions

Non-strictness is a property of a function, it means that the function may choose not to evaluate one or more of its arguments. A strict function
always evaluates its arguments (`&&` and `||` found in many programming languages including Scala, are non-strict).
In Scala, we can write non-strict functions by accepting some of our arguments unevaluated. We can express this by passing a zero-argument function
like `() => A`, where the unevaluated form of an expression is called a thunk, and we can force the thunk to evaluate the expression and get a result.
We're passing a function of no arguments in place of each non-strict parameter, and then explicitly calling this function to obtain a result in
the body. Scala allows to rewrite the previous expression omitting the empty parentheses `theArg: => A`. In the body of the function, we don't need to
do anything special to evaluate an argument annotated with `=>`. We just reference the identifier as usual. The argument passed like this would be
evaluated once for each place it's referenced in the body of the function although you can always store the value of the expression in a `lazy val`
so it is only evaluated one within the body of the function, adding the lazy keyword to a val declaration will cause Scala to delay evaluation of
the right-hand side of that lazy val declaration until it’s first referenced. A non-strict function in Scala takes its arguments by name rather than
by value.

### An extended example: lazy lists

Now we'll see how chains of transformations on streams are fused into a single pass through the use of laziness. We typically want to cache the values
of a Cons node, once they are forced. We avoid this problem by defining smart constructors, which is what we call a function for constructing a data
type that ensures some additional invariant or provides a slightly different signature than the real constructors used for pattern matching. By
convention, smart constructors typically lowercase the first letter of the corresponding data constructor.

### Separating program description from evaluation

A major theme in functional programming is separation of concerns. We want to separate the description of computations from actually running them.
Laziness lets us separate the description of an expression from the evaluation of that expression.

### Infinite streams and corecursion

Because they're incremental, the functions written with lazyness also work for infinite streams: `val ones: Stream[Int] = Stream.cons(1, ones)`.
A corecursive function produces data as oppose as a recursive function, which consumes it. Whereas recursive functions terminate by recursing on
smaller inputs, corecursive functions need not terminate so long as they remain productive, which just means that we can always evaluate more of the
result in a finite amount of time. Corecursion is also sometimes called guarded recursion, and productivity is also sometimes called cotermination. 

## Chapter 6: Purely functional state<a name="Chapter6"></a>