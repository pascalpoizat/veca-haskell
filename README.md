# validation of component architectures (veca-haskell)

[![Build Status](https://img.shields.io/travis/pascalpoizat/veca-haskell/master.svg?style=flat-square)](https://travis-ci.org/pascalpoizat/veca-haskell)
[![Code Coverage](https://img.shields.io/coveralls/pascalpoizat/veca-haskell/master.svg?style=flat-square)](https://coveralls.io/github/pascalpoizat/veca-haskell)
[![License](https://img.shields.io/github/license/pascalpoizat/veca-haskell.svg?style=flat-square)](LICENSE)
[![Version](https://img.shields.io/github/tag/pascalpoizat/veca-haskell.svg?style=flat-square)](veca-haskell.cabal)<br/>
[![Waffle.io - Columns and their card count](https://badge.waffle.io/pascalpoizat/veca-haskell.svg?columns=all)](https://waffle.io/pascalpoizat/veca-haskell)
<!--
[![Version](https://img.shields.io/hackage/v/veca-haskell.svg?label=version&amp;style=flat-square)](https://hackage.haskell.org/package/veca-haskell)
-->

**Unstable API** -- For the time being, it also includes elements relative to the refactoring and generalization of the [FMT project](https://pascalpoizat.github.io/fmt-java) from Java to Haskell.

This is the core engine of the VECA project. 

- for an overview of the VECA project features and objectives, see [VECA project](https://pascalpoizat.github.io/veca-web).

- for the VECA DSL and its integration within the Eclipse IDE, see [veca-ide](https://github.com/pascalpoizat/veca-ide).

## 1. Requirements

You will need a working Haskell environment, which can be obtained using **Stack**.<br/>
See [here](https://haskell-lang.org/get-started) on how to install Stack (more detailed instructions if needed [here](https://docs.haskellstack.org/en/stable/install_and_upgrade/)).

Stack will setup an Haskell environment, including the GHC compiler, if it finds out you need to.
But if you want you can also use Stack to install GHC globally (outside the scope of a project) using `stack setup`.

## 2. Binaries

For the time being, you have to build the veca-haskell executable from sources (see below).

## 3. Building veca-haskell-exe

Once you have cloned the repository, go into the directory and run:

```sh
stack build
stack install
```

This should compile `veca-haskell-exe` and put it in a default directory depending on your OS.
You can use `stack path --local-bin` to find out which directory it is.
Do not forget to put this directory in your command path.

## 4. Using veca-haskell-exe

To get a (basic) help run:

```sh
veca-haskell-exe -h
```

### 4.1 VECA JSON format

The VECA JSON format is not really intended for direct edition, rather use the VECA DSL plugin (see [veca-ide](https://github.com/pascalpoizat/veca-ide)) to generate it.

### 4.2 From VECA DSL to VECA JSON format

The VECA DSL plugin (see [veca-ide](https://github.com/pascalpoizat/veca-ide))
can be used to edit models in the VECA DSL and generate VECA JSON from them. 

### 4.3 VECA to UPPAAL transformation

To transform a VECA model in JSON format into timed automata in XTA format for formal verification, run:

```sh
veca-haskell-exe transform /tmp/myModel
```

provided your model is `/tmp/myModel.json`, and you will get the timed automata in `/tmp/myModel.xta`. Please note that there is no `.json` suffix in the command above.

### 4.4 Formal verification

Formal verification can be achieved using the [ITS-Tools](https://lip6.github.io/ITSTools-web/) or [UPPAAL](http://uppaal.org) verification tools.
