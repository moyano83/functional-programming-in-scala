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
9. [Chapter 9: Parser combinators](#Chapter9)
10. [Chapter 10: Monoids](#Chapter10)
11. [Chapter 11: Monads](#Chapter11)
12. [Chapter 12: Applicative and traversable functors](#Chapter12)
13. [Chapter 13: External effects and I/O](#Chapter13)
14. [Chapter 14: Local effects and mutable state](#Chapter14)

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
scenarios require new combinators. It's a good idea to see if you can refine the combinator you need to its most general form. For example if you
have a combinator that returns the result of an operation passed as an argument, or the result of another operator, then you might want to
generalize the answer for a combinator that chooses between N computations, and make the described combinator a subset of N=2 for this general
combinator. You can try to generalize for different argument types and collections.

## Chapter 8: Property-based testing<a name="Chapter8"></a>

The general idea of property-based testing is to decouple the specification of program behavior from the creation of test cases. The programmer
focuses on specifying the behavior of programs and giving high-level constraints on the test cases; the framework then automatically generates test
cases that satisfy these constraints.

### A brief tour of property-based testing

An example of property-base testing for _ScalaCheck_ would be something like this:

```scala
val intList = Gen.listOf(Gen.choose(0, 100))
val prop = forAll(intList)(ns => ns.reverse.reverse == ns) && forAll(intList)(ns => ns.headOption == ns.reverse.lastOption)
val failingProp = forAll(intList)(ns => ns.reverse == ns) // This is obviously going to fail at some point
// Check the above by using the method check. i.e.
prop.check
```

`intList` is not a `List[Int]`, but a `Gen[List[Int]]`, which is something that knows how to generate test data of type `List[Int]`. The function
`forAll` creates a property by combining a generator of type `Gen[A]` with some predicate of type `A => Boolean`. When we invoke `prop.check`,
ScalaCheck will randomly generate `List[Int]` values to try to find a case that falsifies the predicates supplied.
Other useful features that property testing libraries might come with includes:

    * Test case minimization: If a test fails, the framework tries smaller sizes until it finds the smallest test case that also fails    
    *Exhaustive test case generation: When the domain (set of values that could be produced by some Gen[A]) is small enough we may test all its values

### Choosing data types and functions

#### Initial snippets of an API

Looking at the previous example, we can see `Gen` is a parametric type, and we have two methods for creating generators:

```scala
def listOf[A](a: Gen[A]): Gen[List[A]]
def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] // Different name but same behaviour than Gen.listOf although we specify the size of the list we want
```

We also have a method `forAll` which we can codify as:

```scala
def forAll[A](a: Gen[A])(f: A => Boolean): Prop
```

Where `Prop` is the type result of binding `Gen` with a predicate. We can see from the example above that this type has a method `&&`, so:

```scala
trait Prop {
  def &&(p: Prop): Prop
}
```

#### The meaning and API of properties

With this types above, we can decide what they _mean_. We know `Prop` has `&&` and `check` methods, the later has side effects like printing the
result to the console (must return `Unit` then). But if we return `Unit` we can't really combine the Props in the `&&` method, potentially
throwing away information, so we must return a meaningful type holding the result of the check (which can be as simple as a Boolean).
For the purpose of the example, we will have the trait `trait Prop { def check: Either[(FailedCase, SuccessCount), SuccessCount]}`, where `FailedCase`
can be just a simple String with the description of the parameters that failed the test.

#### The meaning and API of generators

A valid `Gen[A]` implementation might choose to return values of type A by randomly generating these values. Similar to the example of `Rand` in
chapter 6, we could just make Gen a type that wraps a State transition over a random number generator: `case class Gen[A](sample: State[RNG,A])`.
With this type, it is possible to derive the following methods:

```scala
def unit[A](a: => A): Gen[A]
def boolean: Gen[Boolean]
def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]]
```

#### Generators that depend on generated values

Because we might get into a situation where a generator depends on the value produced by other, we need a `def flatMap[B](f: A => Gen[B]): Gen[B]`
operation. Other useful methods we might need includes:

```scala
def listOfN(size: Gen[Int]): Gen[List[A]]
def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] // combining two generators of the same type into one
def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] // similar to union but generates values from each Gen with weighted probability
```

#### Refining the Prop data type

Our Gen representation has revealed information about the requirements for Prop. Prop is missing how many test cases to examine before we consider the
property to have passed the test:

```scala
type TestCases = Int
type Result = Either[(FailedCase, SuccessCount), SuccessCount]

case class Prop(run: TestCases => Result)
```

Because the caller of run learns nothing new by being told the success count, we can omit this transforming the `Either` into an `Option`. Because
`None` is reserved for failed cases (but we are using for the success ones here), we can create a specific type for this instead:

```scala
sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  def isFalsified = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  def isFalsified = true
}
```

The `forAll` method defined before doesn't have enough information to return a Prop. Prop should then have all the information needed to generate
test cases, which means we have to pass the Random generator dependency along:

```scala
case class Prop(run: (TestCases, Rand) => Result)
```

With this in mind, we have all the required information to generate the `forAll` method:

```scala
def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
  (n, rng) =>
    randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
}
def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
def buildMsg[A](s: A, e: Exception): String = s"test: $s\n" + s"exception: ${e.getMessage}\n" + s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
```

### Test case minimization

Ideally we'd like our framework to find the smallest or simplest failing test case, there are two general approaches we could take:

    * Shrinking: After a failing test case, run a separate procedure to minimize the test case by decreasing its size until it no longer fails
    * Sized generation: generate our test cases in order of increasing size and complexity until we find a failure 

Here we'll implement sized generation. Instead of modifying `Gen` data type, we'll introduce sized generation as a separate layer in our library.

```scala
case class SGen[+A](forSize: Int => Gen[A])
```

`SGen` is expecting to be told a size, but `Prop` doesn't receive any size information. We need to add this as a dependency to Prop:

```scala
case class Prop(run: (MaxSize, TestCases, RNG) => Result)
```

### Using the library and improving its usability

Usability is somewhat subjective, but we generally like to have convenient syntax and appropriate helper functions for common usage patterns.

#### Some simple examples

Let's put an example: find the maximum. The maximum of a list should be greater than or equal to every other element in the list:

```scala
val smallInt = Gen.choose(-10, 10)
val maxProp = forAll(listOf(smallInt)) { ns =>
  val max = ns.max
  !ns.exists(_ > max)
}
```

Calling run on this function is cumbersome because the amount of setup that needs to be done, better to create a helper method with some default
values.

### Testing higher-order functions and future directions

We still don't currently have a good way to test higher-order functions. We could take the approach of only examining particular arguments when
testing higher-order functions. For example, the `takeWhile` function:

```scala
val isEven = (i: Int) => i % 2 == 0
val takeWhileProp = Prop.forAll(Gen.listOf(int))(ns => ns.takeWhile(isEven).forall(isEven))
```

There a way we could let the testing framework handle generating functions to use with `takeWhile`. An option is to ignore the input of the
generator and just return constant output, but this is not very efficient.

### The laws of generators

Many of the functions implemented for the `Gen` type look quite similar to other functions we defined on `Par`, `List`, `Stream`, and `Option`.
For example:

```scala
def map[A, B](a: Par[A])(f: A => B): Par[B]
def map[B](f: A => B): Gen[B]
```

Does this functions share similar-looking signatures or do they satisfy the same laws as well? Consider maping with the identity function:

```scala
map(x)(id) == x
```

This law hold for the implementation of `Gen`, `Stream`, `List`, `Option`, and `State`. This proves that these functions share similar-looking
signatures and have analogous meanings in their respective domains.

## Chapter 9: Parser combinators<a name="Chapter9"></a>

Up until now, we have designed our interface first, along with associated laws, and let this guide the choice of data type representations.
_Algebraic design_ is an evolution of this.

### Designing an algebra, first

The laws referred before usually have come after we designed the API and types, but now we'll start with the algebra, including its laws, and decide
on a representation later (known as _algebraic design_). On this example we'll choose to design a library for parsers, focusing on its expressiveness.
The library should generate parser errors if it receives an input it doesn't expect.
A good and simple domain to start with is parsing various combinations of letters like "dsakjdjaks". Let's start with a simple parser for a letter:

```scala
def char(c: Char): Parser[Char]
```

The function returns a parameterized type `Parser` and if it succeeds, we want to get a result that has some useful type, and if it fails, we expect
information about the failure. This parser needs to be run to get a result, so we create a function accordingly:

```scala
def run[A](p: Parser[A])(input: String): Either[ParseError, A]
```

We can make this explicit in a trait:

```scala
trait Parsers[ParseError, Parser[+_]] {
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char]
}
```

In the above, the `[+_]` is the scala notation for a type that is itself a type constructor. Making `ParseError` a type argument lets the `Parsers`
interface work for any representation of `ParseError`, and making `Parser[+_]` a type parameter means that the interface works for any
representation of `Parser`. With this we can define a law `run(char(c))(c.toString) == Right(c)` being 'c' any char.
We can construct a more complex parser for a random string and the above law should still remain valid (adjusting the types obviously).

```scala
def string(s: String): Parser[String]
```

Let's combine this parser with an 'or' function:

```scala
def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
```

And to avoid having to call explicitly the conversion functions, we can create a `ParserOps` class to use implicits to do the transformations:

```scala
trait Parsers[ParseError, Parser[+_]] {
  self =>
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
  }
}
```

What if we want to add repetitions? We can code that like:

```scala
def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] // this should hold true for run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")
```

### A possible algebra

We want to add a new method to add a parser that counts the number of occurrences of a certain character, we start with:

```scala
def many[A](p: Parser[A]): Parser[List[A]]
```

We could have returned a `Parser[Int]`, but we can define a second function that knows how to extract the list length:

```scala
def map[A, B](p: Parser[A])(f: A => B): Parser[B]
```

With the above, we can always pass the result of `many` to `map`, we expect `map` to be _structure preserving_ so we can formalize this by
stipulating the now-familiar law: `map(p)(a => a) == p`.

#### Slicing and nonempty repetition

The combination of `many` and `map` certainly seems inefficient to construct a List[Char] to discard its values and extract its length. We can
construct a `Parser` to see what portion of the input string it examines.

```scala
def slice[A](p: Parser[A]): Parser[String] // run(slice(('a'|'b') .many))("aaba") results in Right("aaba")
```

Note that there's no implementation here yet. We're still just coming up with our desired interface. Now we want to recognize one or more 'a'
characters, so it seems we need some way of running one parser, followed by another, assuming the first is successful. Let's add that:

```scala
// The second parameter is non strict cause if the first Parser fails, the second won't even be consulted. We should change the or function in the 
// same way
def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] 
```

We can try to start implementing the methods (considering the map2 function defined before). We start with `many` which tries running `p`, followed by
`many(p)` again and again until the attempt to parse `p` fails:

```scala
// We want p followed by many(p) again, and that we want to combine their results with :: to construct a list of results
def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) or succeed(List()) 
```

### Handling context sensitivity

Recap the primitives we have so far:

    * string(s): Recognizes and returns a single String
    * slice(p): Returns the portion of input inspected by p if successful
    * succeed(a): Always succeeds with the value a
    * map(p)(f): Applies the function f to the result of p, if successful
    * product(p1,p2): Sequences two parsers, running p1 and then p2, and returns the pair of their results if both succeed
    * or(p1,p2): Chooses between two parsers, first attempting p1, and then p2 if p1 fails

With this operations there are still parsers that we can't implement, for example if the type of second parser depends on the return of the first
one (context sensitive grammar). But similar to previous chapters, we can achieve this with the `flatMap` operation:

```scala
def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]
```

### Writing a JSON parser

Now we'll write a function that produces a JSON parser using only the set of primitives we've defined and any derived combinators.

```scala
def jsonParser[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JSON] = {
  import P._
  val spaces = char(' ').many.slice
  ???
}
```

In FP, it's common to define an algebra and explore its expressiveness without having a concrete implementation.

#### The JSON format

The possible values of JSON datatypes are represented below:

```scala
trait JSON

object JSON {
  case object JNull extends JSON

  case class JNumber(get: Double) extends JSON

  case class JString(get: String) extends JSON

  case class JBool(get: Boolean) extends JSON

  case class JArray(get: IndexedSeq[JSON]) extends JSON

  case class JObject(get: Map[String, JSON]) extends JSON
}
```

### Error reporting

None of the primitives so far let us assign an error message to a parser so we can introduce a primitive combinator for this called label:

```scala
def label[A](msg: String)(p: Parser[A]): Parser[A] // if p fails, its ParseError will incorporate msg
```

We can then have a parser that returns a type containing the error message and Location. But it is wrong to assume that one level of error reporting
will always be sufficient. Let's therefore provide a way to nest labels:

```scala
def scope[A](msg: String)(p: Parser[A]): Parser[A]
```

This second implementation can return a List with the error type, representing a stack of error messages indicating what the Parser was doing when it
failed. We can now revisit our Parsers trait, and simplify it like:

```scala
trait Parsers[Parser[+_]] {
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
}
```

When we have an error that occurs inside an or combinator, we need some way of determining which error(s) to report. For example in an `or` method
call. So we need a primitive for letting the programmer indicate when to commit to a particular parsing branch so we'll allow something like
**try running _p1_ on the input, and if it fails in an uncommitted state, try running _p2_ on the same input; otherwise, report the failure**.
One common solution to this problem is to have all parsers commit by default if they examine at least one character to produce a result.

### Implementing the algebra

#### One possible implementation

We know a parser needs to support the function run `def run[A](p: Parser[A])(input: String): Either[ParseError,A]`

```scala
type Parser[+A] = String => Either[ParseError, A]

def string(s: String): Parser[A] = (input: String) =>
  if (input.startsWith(s)) Right(s)
  else Left(Location(input).toError("Expected: " + s))
```

#### Sequencing parsers

If the parse of an input is successful, then we want to consider those characters consumed and run the next parser on the remaining characters.

```scala
type Parser[+A] = Location => Result[A] // that's either a success or a failure

trait Result[+A]

case class Success[+A](get: A, charsConsumed: Int) extends Result[A] // return a value of type A as well as the number of characters of input consumed

case class Failure(get: ParseError) extends Result[Nothing] //
```

We get at the essence of what a Parser is, it's a kind of state action that can fail. It receives an input state, and if successful, returns a value
as well as enough information to control how the state should be updated.

#### Labeling parsers

In the event of failure, we want to push a new message onto the ParseError stack. We introduce a helper function for this on `ParseError`: `push`

```scala
def push(loc: Location, msg: String): ParseError = copy(stack = (loc, msg) :: stack)
// This allows us to define
def scope[A](msg: String)(p: Parser[A]): Parser[A] = s => p(s).mapError(_.push(s.loc, msg))
```

The function mapError is defined on Result—it just applies a function to the failing case:

```scala
def mapError(f: ParseError => ParseError): Result[A] = this match {
  case Failure(e) => Failure(f(e))
  case _ => this
}
```

#### Failover and backtracking

We said that consuming at least one character should result in a committed parse. We can support the behavior we want by adding one more piece of
information to the `Failure` case of `Result`: a Boolean value indicating whether the parser failed in a committed state:

```scala
case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]
```

The implementation of `or` can simply check the isCommitted flag before running the second parser. In the parser _x_ or _y_, if _x_ succeeds, then the
whole thing succeeds. If _x_ fails in a committed state, we fail early and skip running _y_. Otherwise, if _x_ fails in an uncommitted state, we run
_y_ and ignore the result of _x_:

```scala
def or[A](x: Parser[A], y: => Parser[A]): Parser[A] = s => x(s) match {
  case Failure(e, false) => y(s)
  case r => r
}
```

#### Context-sensitive parsing

`flatMap` enables context-sensitive parsers by allowing the selection of a second parser to depend on the result of the first parser.

```scala
// We define the helper functions
def advanceBy(n: Int): Location = copy(offset = offset + n)

def addCommit(isCommitted: Boolean): Result[A] = this match {
  case Failure(e, c) => Failure(e, c || isCommitted)
  case _ => this
}

def addCommit(isCommitted: Boolean): Result[A] = this match {
  case Failure(e, c) => Failure(e, c || isCommitted)
  case _ => this
}

def flatMap[A, B](f: Parser[A])(g: A => Parser[B]): Parser[B] =
  s => f(s) match {
    case Success(a, n) => g(a)(s.advanceBy(n)) // Advance the source location before calling the second parser.
      .addCommit(n != 0) // Commit if the first parser has consumed any characters.
      .advanceSuccess(n) // If successful, we increment the number of characters consumed by n, to account for characters already consumed by f. 
    case e@Failure(_, _) => e
  }
```

## Chapter 10: Monoids<a name="Chapter10"></a>

A _monoid_ is a simple structure which is defined only by its algebra. Other than satisfying the same laws, instances of the monoid interface may have
little or nothing to do with one another.

### What is a monoid?

A monoid is a structure that complies with the following set of laws:

    * It has an identity operation, for example in a string concatenation, the identity operator is the empty string
    * It is associative, again with the string concatenation (s1+s2+s3) = (s1+s2)+s3 = s1+(s2+s3)

A monoid consist on:

    * Some type A
    * An associative binary operation op, that combines two values of type A: op(op(x,y), z) == op(x, op(y,z)) for any x:A, y:A, z:A
    * A value, zero: A, that is an identity for that operation: op(x, zero) == x and op(zero, x) == x for any x: A

In scala this can be coded like:

```scala
trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}
```

A Monoid it's simply a type A and an implementation of `Monoid[A]` that satisfies the laws.

### Folding lists with monoids

If you look at the signatures of foldLeft and foldRight on List, you might notice something about the argument types:

```scala
def foldRight[B](z: B)(f: (A, B) => B): B // If you replace the A type for B type, it follows the monoid rules
def foldLeft[B](z: B)(f: (B, A) => B): B
```

The components of a monoid fit these arguments. For example with a list of Strings, we could simply pass the op and zero in order to reduce the list
with the monoid and concatenate all the strings. It doesn't matter if we choose foldLeft or foldRight when folding with a monoid, we should get
the same result. This is precisely because the laws of associativity and identity hold. Given this, we can write a general function concatenate
that folds a list with a monoid:

```scala
def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)
def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B // This is an alternative in case A doesn't have a Monoid instance
```

### Associativity and parallelism

The fact that a monoid's operation is associative means we can choose how we fold a data structure like a list. Furthermore, we can reduce a list
using a _balanced fold_, which can be more efficient for some operations and also allows for parallelism, the more balanced tree structure can be
more efficient in cases where the cost of each operation is proportional to the size of its arguments.

### Example: Parallel parsing

Imagine we wanted to count the number of words in a massive file. The strategy would be to split the file into manageable chunks, process several
chunks in parallel, and then combine the results. We need to find a data structure that can handle partial results and can track the complete
words seen so far.

```scala
sealed trait WC

/**
 * simplest case, we haven't seen any complete words yet
 *
 * @param chars the chars of the string
 */
case class Stub(chars: String) extends WC

/**
 * counting over the string "lorem ipsum do" would result in Part ("lorem", 1, "do") since there is no whitespace to the left of lorem or right of 
 * do,we can't be sure if they're complete words, so we don't count them yet.
 *
 * @param lStub holds any partial word we've seen to the left of words
 * @param words keeps the number of complete words we've seen  
 * @param rStub holds any partial word we've seen to the right of words
 */
case class Part(lStub: String, words: Int, rStub: String) extends WC 
```

#### Monoid homomorphisms

There's a law that holds for some functions between monoids called a _monoid homomorphism_. A monoid homomorphism _f_ between monoids _M_ and _N_
obeys the following general law for all values x and y: `M.op(f(x), f(y)) == f(N.op(x, y))`, for example
`"foo".length + "bar".length == ("foo" + "bar").length`.
If two types that your library uses are monoids, and there exist functions between them, it's a good idea to think about whether those functions are
expected to preserve the monoid structure and to check the monoid homomorphism law with automated tests.

### Foldable data structures

When working with data structures, we often don't care about the underlying structure type neither parameter type, we just care about the
operations that allows us to fold, combine, concatenate... it's members, we can capture this on a trait:

```scala
trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B

  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)
}
```

We write it as `F[_]`, where the underscore indicates that _F_ is not a type but a type constructor that takes one type argument. This is called
_higher-order type_ constructor or a _higher-kinded type_.

### Composing monoids

Monoids are _composable_, which means that if types _A_ and _B_ are monoids, then the tuple type (A, B) is also a monoid (called their product).
The fact that multiple monoids can be composed into one means that we can perform multiple calculations simultaneously when folding a data structure.

## Chapter 11: Monads<a name="Chapter11"></a>

### Functors: generalizing the map function

The previous `map` operations defined on the examples before were the same except for the type the operation was applied to. We can capture as a Scala
trait the idea of “a data type that implements map':

```scala
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}
```

The Functor trait is parametric in the choice of F. Here's an instance for List:

```scala
val listFunctor = new Functor[List] {
  def map[A, B](as: List[A])(f: A => B): List[B] = as map f
}
```

We say that a type constructor like `List` or `Option` or `F` is a functor, and the `Functor[F]` instance constitutes proof that F is in fact a
functor. For example, if we have `F[(A, B)]` where F is a functor, we can "distribute" the F over the pair to get `(F[A], F[B])`:

```scala
trait Functor[F[_]] {
  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = (map(fab)(_._1), map(fab)(_._2))
}
```

As it can be seen this is similar to the List's `unzip` function, which means we can write a generic unzip function that works for any functor.
We can also construct the opposite operation over a sum or coproduct:

```scala
def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
  case Left(fa) => map(fa)(Left(_))
  case Right(fb) => map(fb)(Right(_))
}
```

#### Functor laws

Let's consider the laws the functor should have, starting with the identity: `map(x)(a => a) == x` which means that `map(x)` preserves the structure
of _x_. Only the elements of the structure are modified by map; the shape or structure itself is left intact. This kind of algebraic reasoning can
potentially save us a lot of work, since we don't have to write separate tests for these properties.

### Monads: generalizing the flatMap and unit functions

Next, we'll look at a more interesting interface, `Monad`. For several of the data types we've seen (`Parser`, `Gen`, `Par`, `Option`) we have
implemented `map2` to 'lift' a function taking two arguments:

```scala
def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = fa flatMap (a => fb map (b => f(a, b))) // Replace F by Parser, Gen, Par...
```

#### The Monad trait

Given the above, we can come up with a Scala trait for Monad that defines `map2` and numerous other functions, avoiding duplicating their definitions
for every concrete data type. The above map2 function can be generalized in a trait:

```scala
trait Mon[F[_]] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] // Added so the above function uses this methods

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = fa flatMap (a => fb map (b => f(a, b))) // Doesn't compile as we don't know about F
}
```

Notice that we inspected the implementation of map2, and added all the functions it called, map and flatMap, as suitably abstract methods on our
interface. These methods are our set of primitives, that can be refined further. In previous sections `unit` was defined and `map` can be implemented
in terms of `flatMap` and `unit` (these 2 can be our minimal set of primitives):

```scala
trait Mon[F[_]] {
  def unit[A](a: => A): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = fa flatMap (a => fb map (b => f(a, b))) // Doesn't compile as we don't know about F
}

// To tie the above to a concrete data type
object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)

    def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] = ma flatMap f
  }
}
```

### Monadic combinators

Now let's revisit combinators seen on previous chapters and implement them in terms of Monad. For example the `Product` function that was
implemented in terms of `map2`:

```scala
def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))
```

### Monad laws

We know the functor laws to also hold for `Monad`, since a `Monad[F]` is a `Functor[F]`, but there are other laws that constraint `Monad`

#### The associative law

Monads comply with the associative law (holds for all values `x`, `f`, and `g` of the appropriate types):

```scala
x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))
```

#### Proving the associative law for a specific monad: Kleisli composition

There's a way we can prove the law associative law if we consider not the monadic values of types like F[A], but monadic functions of types
like A => F[B]. Functions like that are called Kleisli arrows, and they can be composed with one another:

```scala
def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C]
```

#### The identity laws

There's an identity element for compose in a monad. That's exactly what `unit` is:

```scala
def unit[A](a: => A): F[A]
```

### Just what is a monad?

Usually interfaces provides a relatively complete API for an abstract data type, abstracting over the specific representation. `Monad`, like`Monoid`,
is a more abstract, purely algebraic interface. The Monad combinators are just a small fragment of the full API for a given data type that happens to
be a monad. So Monad doesn't generalize one type or another; rather, many vastly different data types can satisfy the Monad interface and laws. We've
seen three minimal sets of primitive Monad combinators, and instances of Monad will have to provide implementations of one of these sets:

    * unit and flatMap
    * unit and compose 
    * unit, map, and join

Plus there are two monad laws to be satisfied:

    * associativity
    *identity

A monad is an implementation of one of the minimal sets of monadic combinators, satisfying the laws of associativity and identity. A monad is
defined by its operations and laws.

#### The identity monad

The simple case of monad is the identity monad:

```scala
case class Id[A](value: A)
```

The wrapped type and the unwrapped type are totally isomorphic: we can go from one to the other and back again without any loss of information:

```scala
for {
  a <- Id("Hello, ") // This is transformed in a flatMap, which simply extract the value "Hello, "
  b <- Id("monad!")
} yield a + b // returns Id("Hello, monad!")
```

We could say that monads provide a context for introducing and binding variables, and performing variable substitution.

#### The State monad and partial type application

The type `State` seen previously fits the profile for being a monad. But its type constructor takes two type arguments, and Monad requires a type
constructor of one argument. But if we choose some particular _S_, then we have something like `State[S, _]`, which is the kind of thing expected by
`Monad`. So State doesn't just have one monad instance but a whole family of them, one for each choice of _S_:

```scala
type IntState[A] = State[Int, A]

object IntStateMonad extends Monad[IntState] {
  def unit[A](a: => A): IntState[A] = State(s => (a, s))

  def flatMap[A, B](st: IntState[A])(f: A => IntState[B]): IntState[B] =
    st flatMap f
}
```

Scala allows us to use a more terse definition:

```scala
object IntStateMonad extends Monad[({type IntState[A] = State[Int, A]})#IntState] {} // Here we define an anonymous type within the parentheses
```

A type constructor declared inline like this is often called a _type lambda_ in Scala. This anonymous type has, as one of its members, the type alias
IntState, which looks just like before. Outside the parentheses we're then accessing its IntState member with the # syntax.
`State` also had the operations `setState` and `getState` and together with the monadic `unit` and `flatMap` constitutes the set of primitive
operations for this type.
We can use `getState` and `setState` in a for comprehension because at each line in the for-comprehension, the implementation of `flatMap` is
making sure that the current state is available to getState, and that the new state gets propagated to all actions that follow a `setState`.
The Monad contract doesn't specify what happens between the lines, only that whatever happens satisfies the laws of associativity and identity.

## Chapter 12: Applicative and traversable functors<a name="Chapter12"></a>

In this chapter we'll develop a monad for I/O (writing to a file, read from a DB). The IO monad provides a straightforward way of embedding imperative
programming with I/O effects in a pure program while preserving referential transparency. This will illustrate a key technique for dealing with
external effects—using pure functions to compute a description of an effectful computation, which is then executed by a separate interpreter that
actually performs those effects.

### Factoring effects

It is always possible to factor an impure procedure into a pure "core" function and two procedures with side effects: one that supplies the pure
function's input and one that does something with the pure function's output. We can formalize this insight a bit. Given an impure function `f` of
type `A => B`, we can split f into two functions:

    * A pure function of type A => D, where D is some description of the result of f
    * An impure function of type D => B, which can be thought of as an interpreter of these descriptions

### A simple IO type

Procedures like `println` can be factored in much the same way, by introducing a new data type that we'll call IO:

```scala
trait IO {
  def run: Unit
}

def PrintLine(msg: String): IO =
  new IO {
    def run = println(msg)
  }

def func(v1: Value, v2: Value): IO = PrintLine(v1.compareTo(v2))
```

In this example, `func` simply describes an action that needs to take place, but doesn't actually execute it. We say `func` has an _effect_ or is
_effectful_, but it's only the interpreter of `IO` that actually has a side effect. We can define more operations:

```scala
trait IO {
  self =>
  def run: Unit

  def ++(io: IO): IO = new IO {

    def run = {
      self.run;
      io.run
    }
  }
}

object IO {
  def empty: IO = new IO {
    def run = ()
  }
}
```

As it can be seen, IO is now a monoid, with empty as the identity and `++` as the associative operation.

#### Handling input effects

So far the `IO` type can represent only output effects. There's no way to express `IO` computations that waits for input from some external source.
In Scala, `readLine` is a `def` with the side effect of capturing a line of input from the console and returns a String. We could wrap a call to
readLine in `IO`, but we have nowhere to put the result as we don't have a way of representing this sort of effect. The current `IO` type can't
express computations that yield a value. We need to extend our `IO` type to allow input, by adding a type parameter:

```scala
sealed trait IO[A] {
  self =>
  def run: A

  def map[B](f: A => B): IO[B] =
    new IO[B] {
      def run = f(self.run)
    }

  def flatMap[B](f: A => IO[B]): IO[B] =
    new IO[B] {
      def run = f(self.run).run
    }
}
```

We've added map and flatMap functions so IO can be used in for-comprehensions. And IO now forms a Monad:

```scala
object IO extends Monad[IO] {
  def unit[A](a: => A): IO[A] = new IO[A] {
    def run = a
  }

  def flatMap[A, B](fa: IO[A])(f: A => IO[B]) = fa flatMap f

  def apply[A](a: => A): IO[A] = unit(a)
}
```

With this in place, we can define a converter from Farenheit to Celsius that requires the user to input the temperature:

```scala
def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0 / 9.0
def ReadLine: IO[String] = IO {
  readLine
}
def PrintLine(msg: String): IO[Unit] = IO {
  println(msg)
}
def converter: IO[Unit] = for {
  _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
  d <- ReadLine.map(_.toDouble)
  _ <- PrintLine(fahrenheitToCelsius(d).toString)
} yield ()
```

Note that `converter` does not have side effects and it's a referentially transparent description of a computation with effects, and `converter.run`
is the interpreter that will actually execute those effects.

#### Benefits and drawbacks of the simple IO type

The usage of an `IO` monad like what we have so far is important because _clearly separates pure code from impure code_. Some benefits it has   :

    * IO computations are ordinary values. We can store them in lists, pass them to functions, create them dynamically...
    * Seeing IO as values allows for crafting more interesting interpreters than the simple run method baked into the IO type itself.

But this implementation also has its drawbacks:

    * Many IO programs will overflow the runtime call stack and throw a StackOverflowError, specially for larger programs
    * A value of type IO[A] is completely opaque. It's really just a lazy identity, a function that takes no arguments
    * This simple IO[A] has nothing at all to say about concurrency or asynchronous operations 

### Avoiding the StackOverflowError

Consider the program

```scala
val p = IO.forever(PrintLine("Still going..."))
def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] {
  def run = f(self.run).run // creates a new IO object whose run definition calls run again before calling f
}
```

This will keep building up nested run calls on the stack and eventually overflow it.

#### Reifying control flow as data constructors

The solution is surprisingly simple. Instead of letting program control just flow through with function calls, we explicitly bake into our data type
the control flow that we want to support: instead of making `flatMap` a method that constructs a new `IO` in terms of run, we can just make it a data
constructor of the `IO` data type (and we can then use tail recursion).

```scala
sealed trait IO[A] {
  def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)

  def map[B](f: A => B): IO[B] = flatMap(f andThen (Return(_)))
}

// represents an IO action that has finished
case class Return[A](a: A) extends IO[A]

// means that we want to execute some effect to produce a result
case class Suspend[A](resume: () => A) extends IO[A]

// lets us extend or continue an existing computation by using the result of the first computation to produce a second computation
case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B] 
```

This new `IO` type has three data constructors, representing the three different kinds of control flow that we want the interpreter of this data type
to support. An interpreter to evaluate this type of IO can be:

```scala
@annotation.tailrec def run[A](io: IO[A]): A = io match {
  case Return(a) => a
  case Suspend(r) => r()
  case FlatMap(x, f) => x match {
    case Return(a) => run(f(a))
    case Suspend(r) => run(f(r()))
    case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
  }
}
```

Note that instead of saying `run(f(run(x)))` in the `FlatMap(x,f)` case, we instead pattern match on `x`, since it can only be one of three things
and we keep the tail recursion. In order to continue running the program in this last case, we want to do is look at y to see if it is another FlatMap
constructor, but the expression may be arbitrarily deep and we want to remain tail-recursive. We reassociate this to the right, effectively turning
`(y flatMap g) flatMap f` into `y flatMap(a => g(a) flatMap f)`. A function like run is sometimes called a trampoline, and the overall technique
of returning control to a single loop to eliminate the stack is called trampolining.

#### Trampolining: a general solution to stack overflow

The IO type we have so far is a general data structure for trampolining computations, even pure computations that don't do any I/O at all. The
_StackOverflowError_ problem manifests itself in Scala wherever we have a composite function that consists of more function calls than there's
space for on the call stack, but we can solve this with the IO monad:

```scala
sealed trait TailRec[A] {
  def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMap(this, f)

  def map[B](f: A => B): TailRec[B] = flatMap(f andThen (Return(_)))
}

case class Return[A](a: A) extends TailRec[A]

case class Suspend[A](resume: () => A) extends TailRec[A]

case class FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]
```

We can use the `TailRec` data type to add trampolining to any function type `A => B` by modifying the return type B to `TailRec[B]` instead. The
program just had to be modified to use `flatMap` in function composition and to `Suspend` before every function call. Using `TailRec` can be slower
than direct function calls, but its advantage is that we gain predictable stack usage.

### A more nuanced IO type

The above example doesn't solve the problems of not being fit for multithreading (it blocks the current thread on execution) and not being
explicit with the side effects. The second problem can be solved by replacing the `TailRec` type for other parallel types seen before. We can go
even further and use a type constructor for the instead of the type A:

```scala
sealed trait Free[F[_], A]

case class Return[F[_], A](a: A) extends Free[F, A]

case class Suspend[F[_], A](s: F[A]) extends Free[F, A]

case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]
```

#### Reasonably priced monads

The Return and FlatMap constructors witness that this data type is a monad for any choice of F, and since they're exactly the operations required to
generate a monad, we say that it's a free monad. `Free[F,A]` is a recursive structure that contains a value of type `A` wrapped in zero or more
layers of `F`.

#### A monad that supports only console I/O

We can try now to implement an interaction with the console:

```scala
sealed trait Console[A] {
  def toPar: Par[A]

  def toThunk: () => A
}

case object ReadLine extends Console[Option[String]] {
  def toPar = Par.lazyUnit(run)

  def toThunk = () => run

  def run: Option[String] = try Some(readLine()) catch {
    case e: Exception => None
  }
}

case class PrintLine(line: String) extends Console[Unit] {
  def toPar = Par.lazyUnit(println(line))

  def toThunk = () => println(line)
}
```

In the above example, Console can only take the form of input (ReadLine) or Output (PrintLine), which can be used in the `Free` Monad:

```scala
object Console {
  type ConsoleIO[A] = Free[Console, A]

  def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)

  def printLn(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))
}
```

And use it in a program to interact with the console:

```scala
val f1: Free[Console, Option[String]] = for {
  _ <- printLn("I can only interact with the console.")
  ln <- readLn
} yield ln
```

How do we construct a program from this? The signature previously created for _run_ was:

```scala
def run[F[_], A](a: Free[F, A])(implicit F: Monad[F]): F[A]
```

We need a `Monad[F]` that we don't have, so we need to translate the type:

```scala
trait Translate[F[_], G[_]] {
  def apply[A](f: F[A]): G[A]
}

type ~>[F[_], G[_]] = Translate[F, G]

val consoleToFunction0 = new (Console ~> Function0) {
  def apply[A](a: Console[A]) = a.toThunk
}
val consoleToPar = new (Console ~> Par) {
  def apply[A](a: Console[A]) = a.toPar
}
```

So we can generalize _run_:

```scala
def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G)(
  implicit G: Monad[G]): G[A] =
  step(free) match {
    case Return(a) => G.unit(a)
    case Suspend(r) => t(r)
    case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
    case _ => sys.error("Impossible; `step` eliminates these cases")
  }
```

#### Pure interpreters

Note that `ConsoleIO` type does not imply that any effects occurs, that's the responsibility of the interpreter.

### Non-blocking and asynchronous I/O

In the examples before, when we encountered a `Suspend(s)`, `s` will be of type `Console` and we'll have a translation `f` from `Console` to the
target monad. To allow for non-blocking asynchronous I/O, we simply change the target monad from `Function0` to a concurrency monad such as
`scala.concurrent.Future`. In this way we can write both blocking and non-blocking interpreters just by varying the target monad.
A non-blocking source of bytes might have an interface like this:

```scala
trait Source {
  def readBytes(numBytes: Int, callback: Either[Throwable, Array[Byte]] => Unit): Unit // returns inmediately
}
```

### A general-purpose IO type

For any given set of I/O operations that we want to support, we can write an ADT whose case classes represent the individual operations. For any
data type `F`, we can generate a free monad `Free[F,A]` in which to write our programs:

```scala
type IO[A] = Free[Par, A]
```

#### The main program at the end of the universe

When the JVM calls into our main program, it expects a main method with a specific signature. The return type of this method is Unit, meaning that
it's expected to have some side effects. But we can delegate to a pureMain program that's entirely pure. The only thing the main method does in that
case is interpret our pure program, actu- ally performing the effects.

```scala
abstract class App {

  import java.util.concurrent._

  def unsafePerformIO[A](a: IO[A])(pool: ExecutorService): A = Par.run(pool)(run(a)(parMonad))

  def main(args: Array[String]): Unit = {
    val pool = Executors.fixedThreadPool(8)
    unsafePerformIO(pureMain(args))(pool)
  }

  def pureMain(args: IndexedSeq[String]): IO[Unit] // the actual program goes here, as an implementation of pureMain in a subclass of App
}
```

### Why the IO type is insufficient for streaming I/O

Writing efficient, streaming I/O will generally involve monolithic loops, but monolithic loops are not composable.

## Chapter 14: Local effects and mutable state<a name="Chapter14"></a>
