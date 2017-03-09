# VECA in Haskell

[![Build Status](https://img.shields.io/travis/Hariati/vecahaskell/master.svg?style=flat-square)](https://travis-ci.org/Hariati/vecahaskell)
[![Code Coverage](https://img.shields.io/coveralls/Hariati/vecahaskell/master.svg?style=flat-square)](https://coveralls.io/github/Hariati/vecahaskell)
[![License](https://img.shields.io/badge/license-Apache%20License%202.0-blue.svg?style=flat-square)](LICENSE)
[![Version](https://img.shields.io/badge/version-0.1.0.0-orange.svg?style=flat-square&label=version)](vecahaskell.cabal)<br/>
[![Issues Ready](https://img.shields.io/github/issues-raw/Hariati/vecahaskell/ready.svg?style=flat-square&label=issues%20ready%20for%20development)](https://waffle.io/Hariati/vecahaskell)
[![Issues in Progress](https://img.shields.io/github/issues-raw/pascalpoizat/Hariati/in%20progress.svg?style=flat-square&label=issues%20in%20progress)](https://waffle.io/Hariati/vecahaskell)

<!--
[![Version](https://img.shields.io/hackage/v/vecahaskell.svg?label=version&amp;style=flat-square)](https://hackage.haskell.org/package/vecahaskell)
-->

This project is about learning Haskell while developing a DSL for the verification of component architectures (see [VECA](https://github.com/pascalpoizat/veca)).

The idea came while I was coding in Java algorithms that run beyond a DSL plugin written with XText/XTend. These algorithms, and the structures they operate on (mainly trees and transition systems) were written formally in a paper and it was time to implement them.

The Java generics or patterns I needed were quite painful (I may compare the code to transform the information in a tree one day, just for fun) and the typing was not as strong as I would have liked. Then I thought about trying Scala or Kotlin, that have stronger type systems and may operate with Java. But, in the end, why not writing code in a language that is closer to the formalizing in the papers?

## Haskell notes

These notes are prior to a (to appear someday) blog post on installing and running an Haskell development platform on a Mac.

### Documentation

- [Learn you a Haskell for great good!](http://learnyouahaskell.com)
- [Haskell Wiki](https://wiki.haskell.org/FAQ)
- [What I wish I knew when learning Haskell](http://dev.stephendiehl.com/hask/)

### Libraries

- [Hackage central package archive](https://hackage.haskell.org)
- [containers package](https://hackage.haskell.org/package/containers) for various container types such as Set

### Testing

- [Tasty](http://documentup.com/feuerbach/tasty) a testing framework that enables one to combine different kinds of testing (typically, the ones below)
- [HUnit](https://github.com/hspec/HUnit#readme) for unit testing
- [SmallCheck](https://github.com/feuerbach/smallcheck#readme) for property based testing
- [QuickCheck](https://github.com/nick8325/quickcheck#readme) for random based testing
- [HSpec](http://hspec.github.io) BDD testing

### Development

- [Stack](https://haskellstack.org/) build system
- [Cabal](https://www.haskell.org/cabal/) used by Stack
- [Travis CI](https://travis-ci.org) for continuous integration
- [Stack and Travis CI](https://docs.haskellstack.org/en/latest/travis_ci/) for continuous integration
- [HPC](https://wiki.haskell.org/Haskell_program_coverage) for code coverage
- [Coveralls](https://coveralls.io) for code coverage
- [stack-hpc-coveralls](https://github.com/rubik/stack-hpc-coveralls) for code coverage

### IDE

- [Haskell layer](https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Blang/haskell) for [Spacemacs](http://spacemacs.org)

### TODO

see: [issues](https://github.com/pascalpoizat/vecahaskell/issues)

## Acknowledgements

### For help in installing / running Haskell:

This includes Haskell, Stack, and Spacemacs

[@AkiiZedd (twitter)](https://twitter.com/AkiiZedd),
[@BeRewt (twitter)](https://twitter.com/BeRewt),
[@kaddourkardio (twitter)](https://twitter.com/kaddourkardio), 
[@SergeStinckwich (twitter)](https://twitter.com/SergeStinckwich),
[@spacemacs (twitter)](https://twitter.com/spacemacs)

### For help with the Haskell language:

This includes code refactoring / enhancement

[@BeRewt (twitter)](https://twitter.com/BeRewt)

