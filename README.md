[![Build Status](https://travis-ci.org/forsyde/forsyde-shallow.svg?branch=master)](https://travis-ci.org/forsyde/forsyde-shallow)

ForSyDe's Haskell-embedded Domain Specific Language.
====================================================

Description
-----------

The ForSyDe (Formal System Design) methodology has been developed with
the objective to move system design to a higher level of abstraction
and to bridge the abstraction gap by transformational design
refinement.
 
This library provides a shallow implementation of ForSyDe as a Haskell-embedded Domain Specific Language (DSL)

For more information, please see
[ForSyDe's website](http://forsyde.ict.kth.se/).


Installation
------------

The `forsyde-shallow` package is available through
[HackageDB](https://hackage.haskell.org/) and the latest stable
release can be installed via your favorite Haskell package manager,
e.g.:

    cabal update
    cabal install forsyde-shallow
	
To install the latest updates and nightly builds you need clone
[this repository](https://github.com/forsyde/forsyde-shallow). To
install and use the contents of this repository globally, some useful
commands are:

    cabal install -j4 --enable-tests
	cabal configure --enable tests
	cabal test                        # runs the test suites
	cabal haddock                     # generates the API documentation
	ghci                              # starts an interpreter session

To install and use the contents of this repository in a sandbox
environment (recommended), the equivalent commands are:

    cabal sandbox init
    cabal install -j4 --enable-tests
	cabal configure --enable tests
	cabal test                        # runs the test suites
	cabal haddock                     # generates the API documentation
	cabal repl                        # starts an interpreter session with the sandbox loaded


Getting started
---------------

To get started with using `ForSyDe.Shallow`, once succesfully
installed open an interpreter session and load the library:

    > :m +ForSyDe.Shallow
	> let s = signal [1..4] :: Signal Int
    > mooreSY (+) (*2) 0 s
	{0,2,6,12,20}

The example above implements a Moore finite state machine that
calculates the running sum and multiplies the output with 2. For more
examples and tutorials please check the
[forsyde-shallow-examples](https://github.com/forsyde/forsyde-shallow-examples)
repository.

