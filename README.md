# Functional Programming in Scala

## Table of contents:

1. [Chapter 1: What is functional programming?](#Chapter1)
2. [Chapter 2: Getting started with functional programming in Scala](#Chapter2)
3. [Chapter 3: Functional data structures](#Chapter3)
4. [Chapter 4: Handling errors without exceptions](#Chapter4)
5. [Chapter 5: Strictness and laziness](#Chapter5)
6. [Chapter 6: Purely functional state](#Chapter6)
7. [Chapter 7: Purely functional parallelism](#Chapter7)
8. [Chapter 8: Property-based testing](#Chapter8)

## Chapter 1: What is functional programming?<a name="Chapter1"></a>

Functional programming is reasoning in tearms of pure functions, that is, functions that does not have side effects. A formal definition of a pure
function is "a function _f_ with input type _A_ and output type _B_ (written in Scala as a single type: `A => B`, pronounced "A to B" or "A arrow B")
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
the right-hand side of that lazy val declaration until it's first referenced. A non-strict function in Scala takes its arguments by name rather than
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

### Purely functional random number generation

A Random number generator depends on previous invocations of its methods, and therefore it is not easily testable because it is not referentially
transparent. The key to recovering referential transparency is to make the state updates explicit, so instead of updating the state as a side effect,
we simply return the new state along with the value that we're generating. In this way we separate the concern of computing what the next state is
from the concern of communicating the new state to the rest of the program.

```scala
def nextInt(): Int // Instead of mutating the data in place
def nextInt(s: RandomGenerator): (Int, RandomGenerator) // we pass the state along and return it
```

### Making stateful APIs pure

Whenever we use the pattern described above, we make the caller responsible for passing the computed next state through the rest of the program.
This can be tedious, so it is possible to refactor the common code.

### A better API for state actions

Looking at the implementation described, we notice that each of our functions has a type of the form `Generator => (A, Generator)` for some type A.
Functions of this type are called state actions or state transitions because they transform _Generator_ states from one to the next. These state
actions can be combined using combinators, which are higher-order functions. If we define a type alias for these type of functions like
`type Gen[+A] = Generator => (A, Generator)`, we can think of a value of type `Gen[+A]` as a randomly generated A, although it is really a program
that depends on some Generator, uses it to generate an A, and also transitions the Generator to a new state that can be used by another action later.
We can now turn methods such as _nextInt_ into values of this new type: `val int: Gen[Int] = _.nextInt`. We want to write combinators that let us
combine `Gen` actions while avoiding explicitly passing along the `Generator` state. We end up with a kind of domain-specific language that does this
passing for us:

```scala
def unit[A](a: A): Gen[A] = gen => (a, gen)
def map[A, B](s: Gen[A])(f: A => B): Gen[B] = gen => { // Remember Gen[_] is an alias for Generator => (A, Generator)
  val (a, gen2) = s(gen)
  (f(a), gen2)
} 
```

#### Combining state actions

What if we want to combine state actions? we need is a new combinator `map2` that can combine two Gen actions into one using a binary rather than
unary function: `def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C]`.

#### Nesting state actions

If we progress towards an implementation that doesn't explicitly mention or pass along the `Gen` value, we might encounter situations where this is
not viable like if we need to call the function recursively, since we don't have a `Gen` to pass. For this we need to use a combinator that does
this pass for us. `flatMap` allows us to generate a random A with `Gen[A]`, and then take that A and choose a `Gen[B]` based on its value:
`def flatMap[A,B](f: Gen[A])(g: A => Gen[B]): Gen[B]`.

### A general state action data type

The functions (unit, map, map2, flatMap) described above are general-purpose functions for working with state actions, and don't care about the type
of the state, so we can modify map to be more general: `def map[S,A,B](a: S => (A,S))(f: A => B): S => (B,S)`. Given this, we can also modify the
type described before to be more general too: `type State[S,+A] = S => (A,S)`.
Here _State_ is short for computation that carries some state along, or state action, state transition, or even statement. This can be refactored in
its own class like:

```scala
case class State[S, +A](run: S => (A, S))
```

Using this type we can write general-purpose functions for capturing common patterns of stateful programs.

### Purely functional imperative programming

In the imperative programming paradigm, a program is a sequence of statements where each statement may modify the program state. In our case the
"statements" are really State actions. _for-comprehensions_ are a good ally to help to maintain an imperative programming style as oppose to
nested function calls.
To facilitate this kind of imperative programming with for-comprehensions, we only need two primitive `State` combinators: one for reading the
state and one for writing it (get and set).

## Chapter 7: Purely functional parallelism<a name="Chapter7"></a>

### Choosing data types and functions

Imaging you want to sum a list of integers, you can do this with `foldLeft` (sequentially) or using divide and conquer strategy which can be
parallelized:

```scala
def sum(ints: IndexedSeq[Int]): Int =
  if (ints.size <= 1) ints.headOption getOrElse 0
  else {
    val (l, r) = ints.splitAt(ints.length / 2)
    sum(l) + sum(r)
  }
```

#### A data type for parallel computations

From looking at `sum(l) + sum(r)`, we can see that any data type we might choose to represent our parallel computations needs to be able to contain a
result. We can invent a container type for our result, `Par[A]`, and legislate the existence of the functions we need:

`def unit[A](a: => A): Par[A]` for taking an unevaluated A and returning a computation that might evaluate it in a separate thread. We call it
unit because in a sense it creates a unit of parallelism that just wraps a single value

`def get[A](a: Par[A]): A` for extracting the resulting value from a parallel computation

With the above, we can re-write the sum function like:

```scala
def sum(ints: IndexedSeq[Int]): Int =
  if (ints.size <= 1)
    ints.headOption.getOrElse(0)
  else {
    val (l, r) = ints.splitAt(ints.length / 2)
    val sumL: Par[Int] = Par.unit(sum(l))
    val sumR: Par[Int] = Par.unit(sum(r))
    Par.get(sumL) + Par.get(sumR)
  }
```

For this to work, we need to be able to combine asynchronous computations without waiting for them to finish. But in the example above, unit has a
side effect in regards with `get`, which waits for the response.

#### Combining parallel computations

We can replace the `Par.get(sumL) + Par.get(sumR)` with a new operation like `Par.map2(sum(l), sum(r))(_ + _)`, which should be lazy and should begin
immediate execution of both sides in parallel. This also addresses the problem of giving neither side priority over the other.

#### Explicit forking

`def fork[A](a: => Par[A]): Par[A]` is a function that explicitly request the argument to be run in a separate logical thread. This function solves
the problem of instantiating our parallel computations too strictly and it puts the parallelism explicitly under programmer control. With this in
place we can define another function `def lazyUnit[A](a: => A): Par[A] = fork(unit(a))`.
Should evaluation be the responsibility of `fork` or of `get`? Let's rename `get` function to `run`, and dictate that this is where the parallelism
actually gets implemented `def run[A](a: Par[A]): A`. `Par` is now just a pure data structure, `run` has to have some means of implementing the
parallelism, whether it spawns new threads, delegates tasks to a thread pool, or uses some other mechanism.

### Picking a representation

We know run needs to execute asynchronous tasks, there's already a class that we can use in the Java Standard Library
`java.util.concurrent.ExecutorService`.

```scala
class ExecutorService {
  def submit[A](a: Callable[A]): Future[A]
}
```

If we assume that our run function has access to an `ExecutorService` we can rewrite `run` as `def run[A](s: ExecutorService)(a: Par[A]): A`. With
this in mind we can also rewrite the data type `Par[A]` as:

```scala
type Par[A] = ExecutorService => Future[A]
def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
```

### Refining the API

When designing libraries, often a function that seems to be primitive will turn out to be expressible using some more powerful primitive even
implementing operations as new primitives. In some cases, we can even implement the operations more efficiently by assuming something about the
underlying representation of the data types we're working with.

### The algebra of an API

We often get far just by writing down the type signature for an operation we want, and then "following the types" to an implementation. We can
almost forget the concrete domain and just focus on lining up types. We treat the API as an algebra, or an abstract set of operations along with a
set of laws or properties we assume to be true, and simply doing formal symbol manipulation following the rules of the game specified by this algebra.
In functional programming it's easy, and expected, to factor out common functionality into generic, reusable components that can be composed. Side
effects hurt compositionality, but more generally, any hidden or out-of-band assumption or behavior that prevents us from treating our components
as black boxes makes composition difficult or impossible. Review of the sets of algebras you want your API to hold to:

#### The law of mapping

Choosing laws has consequences: it places constraints on what the operations can mean, determines what implementation choices are possible, and
affects what other properties can be true, for example `map(unit(1))(_ + 1) == unit(2)`. Just as we can generalize functions, we can generalize laws.
For example `map(unit(x))(f) == unit(f(x))`. We can define laws in terms of simpler laws that each say just one thing, and we can also determine
what this mapping operation can't do, for example to throw an exception. Given `map(y)(id) ==y`, it must be true that `map(unit(x))(f)==unit(f(x))`.
Since we get this second law or theorem for free, simply because of the parametricity of map, it's sometimes called a free theorem.

#### The law of forking

Fork should not affect the result of a parallel computation: `fork(x) == x`, which means that `fork(x)` should do the same thing as `x`, but
asynchronously, in a logical thread separate from the main thread. If this law didn't always hold, we'd have to somehow know when it was safe to call
without changing meaning, without any help from the type system.

#### Breaking the law: a subtle bug

Is it possible to break the above law? `x` might be some combination of the operations _fork_, _unit_, and _map2_ , but what about `ExecutorService`?
When using an `ExecutorService` backed by a thread pool of bounded size, it's very easy to run into a deadlock. When you find counterexamples like
this, you have two choices—you can try to fix your implementation such that the law holds, or you can refine your law a bit, to state more explicitly
the conditions under which it holds.

### Refining combinators to their most general form

Functional design is an iterative process. After you write down your API and have at least a prototype implementation, you might find that your new
scenarios require new combinators. It’s a good idea to see if you can refine the combinator you need to its most general form. For example if you
have a combinator that returns the result of an operation passed as an argument, or the result of another operator, then you might want to
generalize the answer for a combinator that chooses between N computations, and make the described combinator a subset of N=2 for this general
combinator. You can try to generalize for different argument types and collections.

## Chapter 8: Property-based testing<a name="Chapter8"></a>