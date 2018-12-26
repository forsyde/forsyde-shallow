---
layout: default
title: ForSyDe-Shallow
description: A Framework for Modeling Heterogeneous Systems
isHome: true
---

# Overview

ForSyDe-Shallow is the first and most long-lived incarnation of the ForSyDe modeling concepts, as a shallow-embedded domain specific language (EDSL) in the functional programming language [Haskell](https://www.haskell.org/). It is meant for modeling, simulation and early design validation of heterogeneous embedded and cyber-physical systems, and builds upon the concepts of models of computation (MoC) combined with Haskell's function purity and higher-order functions. 

## Crash Course in ForSyDe Modeling

* A ForSyDe system is modeled as *hierarchical network* of concurrent processes. 
* Each *process* operates with the semantics dictated by a chosen MoC (e.g. Synchronous, Synchronous Dataflow, Scenario Aware Dataflow, Continuous Time, etc.) 
* Processes can only communicate with other processes through *signals*. 
* Processes of different MoCs communicate via *domain interfaces*.

<p align="center">
	<img src="{{ site.url }}/assets/images/forsyde-system-model.svg">
</p>

#### Process

A process takes /m/ input signals as argument and produces /n/ output signals. ForSyDe processes are deterministic.

 * A process is always designed by means of a **process constructor**
 * The process constructor defines the **model of computation** and the **communication interface** of the process
 * The process constructor takes side-effect free **functions** and **variables** as arguments and returns a process

<p align="center"><img src="{{ site.url }}/assets/images/forsyde-process-constructor.svg"></p>
<p align="center"><small>The process constructor <it>mooreSY</it> constructs a Moore FSM process belonging to the synchronous MoC.</small></p>

#### Process Constructor

There are three main categories of process constructors, which exist in all models of computation:

 * **Combinational** process has no internal state
 * **Delay** process delays input
 * **Sequential** processes have an internal state and contain a delay process

The concept of process constructor separates communication from computation thus: 
  * the constructor abstracts communication and interface
  * the function captures computation

This concept also forces the designer to develop a structured formal model that allows for formal analysis, and subsequently enables transformational refinement, implementation mapping and formal verification further down the design process.

# Quick Start

There are [several ways](setup) to acquire the [ForSyDe-Shallow]({{ site.github.baseurl }}) libraries, however the easiest one is to use a Haskell package manager to grab directly from [HackageDB](https://hackage.haskell.org/). Make sure you have the [Haskell Platform](https://www.haskell.org/platform/) installed on your machine type in the command

	cabal update
    cabal install forsyde-shallow

After the installation succeeds, you can test the libraries by starting an interpreter session with 

    ghci
	
Inside an interpreter session you can load the ForSyDe-Shallow libraries and use the provided functions and constructors to test their behavior, for example: 

    > :m +ForSyDe.Shallow
	> let s = signal [1..4] :: Signal Int
	> mooreSY (+) (*2) 0 s
	{0,2,6,12,20}

The example above implements a Moore finite state machine that calculates the running sum and multiplies the output with 2. For a more detailed step-by-step tutorial, please follow the [getting started example](getting_started).

# Documentation and resources

Here you can find links to further documentation resources:

 * [**The setup page**](setup) contains detailed instructions on how to install and use the libraries.
 
 * [**A getting started tutorial**](getting_started) covers a step-by-step example to getting used to the modeling framework.

 * [**The API documentation**](http://hackage.haskell.org/package/forsyde-shallow) generated with Haddock and hosted on [HackageDB](https://hackage.haskell.org/).

 * [**The `forsyde-shallow-examples` repository**](https://github.com/forsyde/forsyde-shallow-examples) contain examples and documented experiments.
