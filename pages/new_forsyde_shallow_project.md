---
layout: default
title: Starting a new ForSyDe-Shallow project
permalink: new_forsyde_shallow_project.html
---

# Starting a new ForSyDe-Shallow project

This short tutorial describes how to start a new ForSyDe-Shallow project for the cross-platform build-tool [Stack](https://docs.haskellstack.org/) and can make use from editors that support the [language server protocol](https://microsoft.github.io/language-server-protocol/). In this example, we use Linux and the editor `emacs`. Please, adapt the tutorial to your tool environment.

[This demo](https://github.com/haskell/haskell-language-server/tree/master/plugins/hls-eval-plugin) show some of the capabilities of the Haskell Language Server.

## Preparation: Installation of the Haskell tools

Setup the Haskell environment as outlined on the [Get started tutorial](https://www.haskell.org/get-started/) on the web pages for the [Haskell language](https://www.haskell.org). 

## Create a new ForSyDe project with Stack

**NOTE:** The following steps will result in a ForSyDe project structure. If used with an editor that supports the  [language server protocol](https://microsoft.github.io/language-server-protocol/), a lot of useful information will be received from the editor during development time. The final outcome  of the following steps will result in a project structure similar to [this one]({{ parent-url }}/assets/forsyde-project.zip), where the copyright and license information has been removed. For a new project, please follow the steps below and do not just copy the zip-file. This ensures that you also have the correct copyright and license information. For this tutorial, we have used the `ghc`-version 9.4.8 (LTS 21.25).

1. Create a new Haskell-project with the Haskell build tool [Stack](https://docs.haskellstack.org). You can choose any name, here `forsyde-project` is chosen as project name.
```
prompt> stack new forsyde-project
```
2. Enter the project directory
```
prompt> cd forsyde-project/
prompt> ls
app  CHANGELOG.md  forsyde-project.cabal  LICENSE  package.yaml  README.md  Setup.hs  src  stack.yaml  test
```
3. Open the file `package.yaml` in your editor and add a line `- forsyde-shallow >= 3.5.0.0` under `dependencies:`.
```
dependencies:
- base >= 4.7 && < 5
- forsyde-shallow >= 3.5.0.0
```
4. Open the file `stack.yaml` in your editor. Remove the bracket `#` before `extra-deps:`, remove `[]` and add 
a line `- forsyde-shallow-3.5.0.0` under `extra-deps:`
```
extra-deps: 
- forsyde-shallow-3.5.0.0
```
5. Create a Haskell file for your new ForSyDe model. 
```
emacs src/ForSyDeModel.hs
```
6. Open the file `src/ForSyDeModel.hs` and enter the following code: 
   ```haskell
   module ForSyDeModel where

   import ForSyDe.Shallow

   adder = zipWithSY (+)
   ``` 
7. Assuming you use an editor that uses the language server protocol (LSP). If the module `ForSyDe.Shallow` is not found, check that your global configuration uses the same [stackage release version](https://www.stackage.org/#about). Open the global stack project file (**NOTE**: Here we give the location for Linux, however, for other operating systems the location might differ, check on the [Stack YAML configuration page](https://docs.haskellstack.org/en/stable/yaml_configuration/), where the global configuration file is stored)
```
prompt> less ~/.stack/global-project/stack.yaml
...
packages: []
resolver: lts-21.25
...
```
and check which stackage release is used. Check the `resolver` entry. For the LSP-protocol, the editor will work with the global stack configuration, so it is important that the global and the project stackage version are identical. 

	If you have created a new project, very likely the stack version of the local project file needs to be changed. So, in this case check the local `stack.yaml` file and check under `resolver: ` which version is used.
	```
	prompt> 
	...
	resolver:
		url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/22/5.yaml
	...
	```
	Change the `resolver` entry to the version in your global stack configuration file and save it. In our case the new resolver entry in the local `stack.yaml` file needs to be the following.
```
...
resolver: lts-21.25
...
```
8. The editor might still not detect the module `ForSyDe.Shallow` using the haskell-language-server with your editor, it is often necessary to generate a `hie.yaml` file. This can be done by running the program `gen-hie`, which can be installed with `stack`. 
   ```
   stack install implicit-hie
   ```
Then in the top-level of your project, you can generate the `hie.yaml` file.
    ```
    gen-hie > hie.yaml
	```
Now, the editor should be able to detect the `ForSyDe.Shallow` module.	
9. Close the editor and reopen your ForSyDe model `src/ForSyDeModel.hs`. For VSCode it is also required that you run in 'Trusted Mode', please double-check that you do not run in restricted mode.

	You should now be able to see some suggestions in the editor. If you hover about Haskell or ForSyDe functions and will see more detailed information about these functions.
10. **Cleaning (Optional)**: When creating a new project with the command `stack new`, additional directories and files have been created which are not needed for modelling a ForSyDe system, which we normally create as a library and not as an executable. To remove the unnecessary directories and files, the following needs to be done.
	 * Remove the unnecessary directories and files.
	  ```
	  prompt> rm src/Lib.hs
	  prompt> rm -rf app
	  ```
	 * Open `package.yaml` and remove the complete entry `executables:` and its content.
