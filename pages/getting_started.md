---
layout: default
title: Getting Started with ForSyDe-Shallow
permalink: getting_started.html
---

# Getting Started with ForSyDe-Shallow

## Introduction

ForSyDe (Formal System Design) has been developed as system design methodology for heterogeneous embedded systems. The initial idea of ForSyDe has been to start with an abstract and formal specification model, that is then refined using well-defined design transformations into a low-level implementation model, and finally mapped into an implementation in hardware or software [[Sander and Jantsch, 2004]]({{ site.parent-url }}/publications.html#2004). Initially ForSyDe only used the synchronous model of computation (MoC), but has later been extended to cover additional models of computation, which can be integrated into one executable heterogeneous model of computation. For the synchronous model of computation there exists a synthesis back-end, which translates an executable synchronous ForSyDe model into the corresponding VHDL-code that can then further be synthesized using a commercial logic synthesis tool.

The tutorial focuses mainly on the modeling concepts of ForSyDe. We will explain the concepts using the synchronous model of computation, but the main concepts apply to all other supported ForSyDe models of computation as well. The tutorial has been written in such a way that knowledge of the functional programming language Haskell should not be required. However, in order to design systems in ForSyDe good knowledge of the main Haskell concepts is needed. For more information on Haskell consult the ​[Haskell web page](https://www.haskell.org/), where you find a lot of information and links to books and tutorials. For more information on ForSyDe consult the ForSyDe page.

### Installing ForSyDe-Shallow

To install the ForSyDe-Shallow library, we assume that you have installed ForSyDe-Shallow according to the Quick Start Instructions on the [ForSyDe-Shallow overview page]({{ parent-url }}/index#quick-start).

## System Modeling in ForSyDe
In contrast to other approaches based on functional languages, ForSyDe has been designed to be able to specify systems at a high level of abstraction. However, we start with simple examples of the hardware world in order to introduce the main ForSyDe concepts.

If you have installed ForSyDe, start the GHC interpreter `ghci` and add the module `ForSyDe.Shallow`:
```
myprompt> stack ghci --package forsyde-shallow
...
ghci>
```	
In case you want to change the prompt, you can do it by the following command, where you can have your own definition of the prompt. The following code would give the prompt `ghci> `.
```
:set prompt "ghci> "
```
The load the ForSyDe-Shallow library.
```
ghci>  :m +ForSyDe.Shallow
```
### Signals

Systems are modeled in ForSyDe by concurrent processes that interact via signals. Signals are similar to lists and can be created using the function signal that converts a list to a signal.
```
ghci> s1 = signal [1,2,3]
ghci> s2 = signal [2,3,4]
```
Signals are represented with curly brackets.
```
ghci> s1
{1,2,3}
ghci> s2
{2,3,4}
```
### Combinational Processes

Processes are functions that take input signals and produce output signals. An adder can be modeled as a process in ForSyDe using the following code.
```
ghci> adder in1 in2 = zipWithSY (+) in1 in2
```
Here `adder` is a process that takes two input signals `in1` and `in2` and produces an output signal. We can simulate the adder by applying the signals `in1` and `in2` to the process `adder`.
```
ghci> adder s1 s2
{3,5,7}
```
If we look closer to the definition of the adder, the adder is constructed by a *process constructor* zipWithSY and a function `(+)`. The concept of process constructor is central in ForSyDe. All processes in ForSyDe are created using process constructors.

The process constructor `zipWithSY` belongs to the synchronous model of computation as indicated by the suffix `SY`. It defines the model of computation and the interface of the process. Process constructors are functions that take functions as arguments and produce another function as output. Such functions are called higher-order-functions in the functional programming community. Here, the process constructor `zipWithSY` takes a function `(+)` as argument, and produces a process `adder` as output. The concept of process constructor separates *communication*, provided by the process constructor, from *computation*, provided by the function. The names of the process constructors in ForSyDe originate from Haskell and many other functional languages, where higher-order-functions like `map` and `zipWith` operate on lists.

The concept of process constructor is very general. We can use `zipWithSY` to create other synchronous combinational processes, which takes two input signals and one output signal. Here follows another example.
```
and2 = zipWithSY (&&)
```	
Please observe that we did not provide the input signals when we declare the process `and2`. In fact we could have declared the adder in the same way without providing the input signals.
```
adder' = zipWithSY (+)
```
We can simulate the new process `and2` with input signals of the required type.
```
ghci> s3 = signal [True, False, True]
ghci> s4 = signal [False, True, True]
ghci> and2 s3 s4
{False,False,True}
```	
As you may have observed, the process `adder` operates on signals of numerical data types, while the process `and2` operates on signals of Boolean. This could give the impression that Haskell has a weak or dynamic type system. However, this is not true. Haskell has a static and strong type system, which infers the type of the functions. We can ask for the type of a function using the `:t` command in `ghci`.
```
hci> :t adder
adder :: Num c => Signal c -> Signal c -> Signal c
ghci> :t and2
and2 :: Signal Bool -> Signal Bool -> Signal Bool
```	
We can read the information provided by `ghci` as follows. The process `adder` takes a signal of data type `a` as first argument, a signal of data type `a` as second argument, and produces a signal of data type `a` as result. A further requirement is that the data type `a` needs to belong to the class of numerical values `Num`. Thus the process `adder` can also be applied on signals of real numbers.
```
ghci> s5 = signal [1.0, 2.0, 3.0]
ghci> s6 = signal [2.0, 3.0, 4.0]
ghci> adder s5 s6
{3.0,5.0,7.0}
```	
It is also possible to define types for signals or functions. The signal `s7` is declared as a signal of integers.
```
s7 = signal [1,2,3] :: Signal Int
```
Haskell’s type system will detect type inconsistencies. So it will complain, if `adder` is applied to one signal of integers and another signal of real numbers, since these are two different data types.
```
ghci> adder s1 s7
{2,4,6}
ghci> adder s5 s7

<interactive>:32:7: error:
    • No instance for (Fractional Int) arising from a use of ‘s5’
    • In the first argument of ‘adder’, namely ‘s5’
      In the expression: adder s5 s7
      In an equation for ‘it’: it = adder s5 s7
```
We can now take a look on the type declaration of `zipWithSY`.
```
ghci> :t zipWithSY
zipWithSY :: (a -> b -> c) -> Signal a -> Signal b -> Signal c
```
The process constructor `zipWithSY` takes a function as first argument. This function has two arguments, the first one of a data type `a` and the second one of a data type `b`. It produces a result of a data type `c`. The second argument of `zipWithSY` is a signal of data type `a`, and the third argument is a signal of data type `b`. `zipWithSY` produces as result a signal of data type `c`. Observe that the data types `a`, `b`, and `c` may be different, but do not have to be, as in the case of `adder` and `and2`.

Since process constructors can take any function that complies to the type declaration, there is no need for a large set of process constructors. For the synchronous model of computation, the following set of combinational process constructors is defined.
```haskell
mapSY      :: (a -> b) -> Signal a -> Signal b
zipWithSY  :: (a -> b -> c) -> Signal a -> Signal b -> Signal c
zipWith3SY :: (a -> b -> c -> d) -> Signal a -> Signal b -> Signal c -> Signal d
zipWith4SY :: (a -> b -> c -> d -> e) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
```
ForSyDe-Shallow also defines _aliases_ for the combinational process constructores `mapSY` (`combSY`), `zipWithSY` (`comb2SY`), `zipWith3SY` (`comb3SY`), and `zipWith4SY` (`comb4SY`), which might be more intuitive for industrial users. Thus, an adder with three input signals could also be defined in the following way.
```
ghci> adder'' = comb2SY (+)
ghci> adder'' s1 s2
{3,5,7}
```
So far we have in an interactive way provided the input for `ghci`, but the natural way to specify the models is to use files. The file [`GettingStarted.hs`]({{site.url}}/assets/GettingStarted.hs) provides the code for the adder and some input signals. Observe that you
* should have a module name that is identical to the file name
* need to import the module `ForSyDe.Shallow`
* shall not have the let statement before function and signal declaration.

{% highlight haskell %}
module GettingStarted where

import ForSyDe.Shallow

adder in1 in2 = zipWithSY (+) in1 in2

s1 = signal [1,2,3]
s2 = signal [2,3,4]
{% endhighlight %}

You can now load the file into ghci using the command `:l`.
```
hci> :l GettingStarted.hs 
[1 of 1] Compiling GettingStarted   ( GettingStarted.hs, interpreted )
Ok, one module loaded.
```
and execute the `adder` that is defined in `GettingStarted.hs`.
```
ghci> adder s1 s2
{3,5,7}
```
You can reload your program with the command `:r`. For other commands use `:h`, which displays the help menu.

### Sequential Processes

ForSyDe also provides process constructors for sequential processes. The basic process constructor is `delaySY`, which delays an input signal one event cycle.
```
ghci> s1
{1,2,3}
ghci> delaySY 0 s1
{0,1,2,3}
```
	
In the implementation of ForSyDe we have chosen that `delaySY` outputs one more event than the corresponding input signal, although this could be regarded as non-causal (more output events are produced than input events are consumed). However, the additional output event can be predicted, and gives also a clear advantage when dealing with feedback loops, since no extra delay needs to be inserted.

There exists a number of process constructors for finite state machines. `scanldSY` models a finite state machine without output decoder. It takes a function to calculate the next state as first argument, a value for the initial state as second argument, and creates a process with one input and one output signal.

The following program implements a counter that counts in both directions between 0 and 4.

{% highlight haskell %}
data Direction = UP
               | HOLD 
               | DOWN deriving (Show)

counter dir = scanldSY count 0 dir

count state HOLD = state
count 4     UP   = 0
count state UP   = state + 1
count 0     DOWN = 4
count state DOWN = state - 1
{% endhighlight %}

Direction is an enumerated data type with the values `UP` and `DOWN`. The process counter is modeled with the process constructor `scanldSY`. It takes a function count and an initial state value `0` as arguments. The function `count` uses pattern matching. The first row of `count` reads as follows. If the direction has the value `HOLD`, the state will not change. The second row matches, if the state is `4` and the direction is `UP`. In this case the next state will be `0`. The third row matches all other patterns for state, if the direction is `UP`. In this case, the state is incremented by one. The fourth and fifth row give the corresponding functionality for the direction `DOWN`.
```
ghci> counter (signal [UP,UP,UP,HOLD,UP,UP,DOWN,DOWN])
{0,1,2,3,3,4,0,4,3}
```
More complex finite state machines can be designed using the process constructors `mooreSY` and `mealySY` for finite state machines with an output decoder. In a Moore FSM, the output signal only depends on the state, while in a Mealy FSM, the state also depends on the current input.

In the following the types of the main sequential process constructors for one and two input values are given.

{% highlight haskell %}
delaySY :: a -> Signal a -> Signal a
scanldSY :: (a -> b -> a) -> a -> Signal b -> Signal a
mooreSY :: (a -> b -> a) -> (a -> c) -> a -> Signal b -> Signal c
mealySY :: (a -> b -> a) -> (a -> b -> c) -> a -> Signal b -> Signal c
scanld2SY :: (a -> b -> c -> a) -> a -> Signal b -> Signal c -> Signal a
moore2SY :: (a -> b -> c -> a) -> (a -> d) -> a -> Signal b -> Signal c -> Signal d
mealy2SY :: (a -> b -> c -> a) -> (a -> b -> c -> d) -> a -> Signal b -> Signal c -> Signal d
{% endhighlight %}

### Process Networks

In the following we model a binary counter as a Mealy FSM and use it to connect several binary counters to an *n*-bit-counter that counts in both directions from 0 to 2<sup>n</sup>.

The binary counter is modeled using the process constructor `mealySY`, a function for the next state decoder `binCount`, another function for the output decoder `carry`, and an initial state value `0`. The last row in the `carry` function uses `_`, which expresses a don't-care pattern.

{% highlight haskell %}
binCounter = mealySY binCount carry 0

binCount state HOLD = state
binCount 0     UP   = 1
binCount 1     UP   = 0
binCount 0     DOWN = 1
binCount 1     DOWN = 0

carry 1 UP   = UP
carry 0 DOWN = DOWN
carry _ _    = HOLD
{% endhighlight %}

There are many ways to create a process network of several processes. The most general is to construct a process network as a net-list, where the net-list is specified as a set of equations using a `where`-clause. So a three-bit counter can be modeled as follows, where `s1` and `s2` are internal signals.

{% highlight haskell %}
counter3Bit dir = out
   where out = binCounter s2
         s2  = binCounter s1
         s1  = binCounter dir
{% endhighlight %}

For simulation it is often useful to define signals of infinite length. Since Haskell is a language that uses lazy evaluation, an infinite signal can be defined recursively. The signal `ups` is an infinite signal, where all events have the value `UP`.

{% highlight haskell %}
ups = UP :- ups
{% endhighlight %}

Since Haskell is a lazy language, only the part of the signal that is needed will be executed. The ForSyDe function `takeS` will return the first *n* values of the signal. So, we can for instance execute the 3-bit counter for 10 cycles.
```
ghci> takeS 10 (counter3Bit ups)
{HOLD,HOLD,HOLD,HOLD,HOLD,HOLD,HOLD,UP,HOLD,HOLD}
```	
However, ForSyDe offers also function composition, which is a basic operation in Haskell to create a new function be connecting two functions. The function composition operator ∘ is written in Haskell as a dot `.`. It is defined as follows.

{% highlight haskell %}
(.) :: (b -> c) -> (a -> b) -> a -> c
(f . g) x = f(g (x))
{% endhighlight %}

Using this operator we can create a two-bit counter by composition of two binary counters.

{% highlight haskell %}
counter2Bit = binCounter . binCounter
{% endhighlight %}

Another very useful operator is the function application operator `$`. It is defined as

{% highlight haskell %}
f $ x = f x
{% endhighlight %}

At first sight, it looks pretty useless. However, it makes the program more readable, since usual function application is left-associative, while function application with `$` is right-associative.

The following two process networks `network1` and `network2` are equivalent:

{% highlight haskell %}
network1 in1 in2 = mapSY even (zipWithSY (+) in1 in2)
network2 in1 in2 = mapSY even $ zipWithSY (+) in1 in2
{% endhighlight %}

Using more advanced features of Haskell, we can now express a general *n*-bit counter.

{% highlight haskell %}
counterNBit n = foldl (.) id $ replicate n binCounter
{% endhighlight %}

The function `replicate` creates a list of *n* `binCounter` processes. Remember that functions - a ForSyDe process is a function - are first-class data types in Haskell and thus they can be part of a list. Then the higher order function `foldl` composes our *n* binary counters together with an identity function `id` into an *n*-bit counter.
```
ghci> counterNBit 2 $ takeS 12 ups
{HOLD,HOLD,HOLD,UP,HOLD,HOLD,HOLD,UP,HOLD,HOLD,HOLD,UP}
ghci> counterNBit 3 $ takeS 12 ups
{HOLD,HOLD,HOLD,HOLD,HOLD,HOLD,HOLD,UP,HOLD,HOLD,HOLD,HOLD}
```	
Finally, since Haskell allows partial function application, we can create fixed-size counters using the generic *n*-bit counters.

{% highlight haskell %}
counter1Bit' = counterNBit 1
counter2Bit' = counterNBit 2
counter3Bit' = counterNBit 3
{% endhighlight %}

The counters execute as expected.
```
ghci> counter1Bit' $ takeS 12 ups
{HOLD,UP,HOLD,UP,HOLD,UP,HOLD,UP,HOLD,UP,HOLD,UP}
ghci> counter2Bit' $ takeS 12 ups
{HOLD,HOLD,HOLD,UP,HOLD,HOLD,HOLD,UP,HOLD,HOLD,HOLD,UP}
ghci> counter3Bit' $ takeS 12 ups
{HOLD,HOLD,HOLD,HOLD,HOLD,HOLD,HOLD,UP,HOLD,HOLD,HOLD,HOLD}
```
