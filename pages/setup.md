---
layout: default
title: Installation and usage
permalink: setup.html
---

# Dependencies

Before installing, you need to take care of the following dependencies:

### General dependencies

These are required to acquire, install and use the base libraries:

 * The [Haskell Platform](https://www.haskell.org/platform/). The libraries usually support the latest `ghc` versions, but you can check the tested versions in the   [`forsyde-shallow.cabal`](https://github.com/forsyde/forsyde-shallow/blob/master/forsyde-shallow.cabal) file in case the installation does not succeed.
 
 * [Git](https://git-scm.com/downloads) if you want to clone the whole repository, and not just download the sources. 
 
Library dependencies are taken care of by the [Cabal](https://www.haskell.org/cabal/) or [Stack](https://docs.haskellstack.org/en/stable/README/) package managers shipped with [Haskell Platform](https://www.haskell.org/platform/).

### Plotting the signals

[The CT MoC library](http://hackage.haskell.org/package/forsyde-shallow-3.3.2.0/docs/ForSyDe-Shallow-MoC-CT.html) provides helper functions for plotting signals using the [gnuplot](http://gnuplot.info/) engine. You need to have it installed for your OS.

For an OS using the [X Window System](https://en.wikipedia.org/wiki/X_Window_System) e.g. Ubuntu Linux, you might need to install the [`gnuplot-x11`](http://gnuplot.sourceforge.net/docs_4.2/node442.html) library, to be able to launch plots from an interpreter session. For example, in Ubuntu, you need to type in:

    sudo apt instal gnuplot-x11
	
# Installation

Before trying to install, check the list of [dependencies](#dependencies) above, to see that you meet the requirements based on how you are planning to use the library.

The [`forsyde-shallow`](https://hackage.haskell.org/package/forsyde-shallow)
package is available through [HackageDB](https://hackage.haskell.org/)
and the latest stable release can be installed via your favorite
Haskell package manager.

### Using Stack

The easiest way to getting started is by using the
[Stack](https://docs.haskellstack.org/en/stable/README/) package
manager, which takes care of fetching and installing an appropriate
version of the Haskell compiler, the dependent packages, and sets
everything up in a sandboxed environment.

    stack update
    stack upgrade
    stack install forsyde-shallow
    stack ghci      # starts an interpreter session
	
To install the latest updates and nightly builds you need clone
[this repository](https://github.com/forsyde/forsyde-shallow). To
install and use the contents of this repository globally, some useful
commands are:

    stack install
	stack test                        # runs the test suites
	stack haddock                     # generates the API documentation
	stack ghci --no-load              # starts an interpreter session, option given to avoid pre-loading all modules

### Using Cabal

Alternatively, you can use the [Cabal](https://www.haskell.org/cabal/)
package manager, but then you need to take care of acquiring an
appropriate Haskell tool suite which includes the GHC compiler and the
`cabal-install` package.

    cabal update
    cabal install forsyde-shallow
    ghci
	
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

	
### Testing the libraries

The test suite and its dependencies can be installed and run by
explicitly adding the flag `--enable-tests` to the previous
installation commands, namely:

    cabal install --enable-tests --dependencies-only # (optional)
    cabal install --enable-tests                     # installs library + test suite
    cabal configure --enable-tests                   # configures the package to run the test suite
    cabal test                                       # runs the test suite 
      
# Generating the API documentation locally

The API documentation for latest release is publicly available [here](http://hackage.haskell.org/package/forsyde-shallow) but if for some reason you need to generate it locally on your machine you need perform the following steps:

 1. install the [`hscolour`](https://hackage.haskell.org/package/hscolour) Haskell package
     
	    cabal install hscolour
	 
 1. `cd` into the root of the project (the folder containing the `forsyde-shallow.cabal` file) and type in:
     
	    cabal haddock --hyperlink-source


