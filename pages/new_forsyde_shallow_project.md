---
layout: default
title: Starting a new ForSyDe-Shallow project
permalink: new_forsyde_shallow_project.html
---

# Starting a new ForSyDe-Shallow project

This short tutorial describes how to start a new ForSyDe-Shallow project for the cross-platform build-tool [Stack](https://docs.haskellstack.org/) and can make use from editors that support the [language server protocol](https://microsoft.github.io/language-server-protocol/). In this example, we use Linux and the editor `emacs`. Please, adapt the tutorial to your tool environment.

## Installation of the Haskell tools

[Setup the Haskell environment](https://www.haskell.org/get-started/) as outlined on the web page. 

## Create a new Stack project

1. Create a new Haskell-project with `stack`. You can choose any name, here `forsyde-project` is chosen as project name.
```
prompt> stack new forsyde-project
```
2. Enter the project directory
```
prompt> cd forsyde-project/
prompt> ls
app  CHANGELOG.md  forsyde-project.cabal  LICENSE  package.yaml  README.md  Setup.hs  src  stack.yaml  test
```
3. Open the file `package.yaml` and add `- forsyde-shallow >= 3.5.0.0` under `dependencies:`.
```
dependencies:
- base >= 4.7 && < 5
- forsyde-shallow >= 3.5.0.0
```
4. Open the file `stack.yaml` and add `- forsyde-shallow-3.5.0.0` under `extra-deps:`
```
extra-deps: 
- forsyde-shallow-3.5.0.0
```
5. Remove the file `src/Lib.hs` and create another Haskell file for your new ForSyDe model. 
```
rm src/Lib.hs
emacs src/ForSyDeModel.hs
```
6. Open the file `src/ForSyDeModel.hs` and enter the following code:
```haskell
module ForSyDeModel where

import ForSyDe.Shallow

adder = mapSY (+)
```
7. Assuming you use and editor that uses the language server protocol (LSP). If the module `ForSyDe.Shallow` is not found, check that your global configuration uses the same [stackage release version](https://www.stackage.org/#about). Open the global stack project file
```
prompt> less ~/.stack/global-project/stack.yaml`
...
packages: []
resolver: lts-21.25
...
```
and check which stackage release is used. Check the `resolver` entry. For the LSP-protocol, the editor will work with the global stack configuration, so it is important that the global and the project stackage version are identical. 

If you have created a new project, very likely the stack version of the local project file needs to be changed. So, in this case check the local `stack.yaml` file and check under `resolver: `, which version is used.
```
prompt> 
...
resolver:
  url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/22/5.yaml
...
```
requires that the same entry e the same as under your project's `stack.yaml` file. Change the `resolver` entry to the version in your global stack configuration file and save it. In our case the new resolver entry will be the following.
```
...
resolver: lts-21.25
...
```
