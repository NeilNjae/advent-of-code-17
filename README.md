---
title: "Advent of Code 2017"
output: html_document
css: modest.css
---
Code to solve the [Advent of Code](http://adventofcode.com/2017/) puzzles. This year, I'm using the puzzles to develop my skills in [Haskell](https://wiki.haskell.org/Haskell).

[Learn you a Haskell](http://learnyouahaskell.com/chapters), [Introduction to Haskell 98](https://www.haskell.org/tutorial/index.html), and [Hackage](https://hackage.haskell.org/) are good resources.

# Toolchain

I'm using the basic Haskell Platform installation, togeher with `Stack` to manage the packages and dependencies (install with
```
$ sudo aptitude install haskell-platform haskell-stack
```
).

## Creating the repository and project
Create the repository as normal: create the project in Gitolite, clone it, and insert the `.gitignore` and `README.md` files.

Within the project directory, there will be one package for each day. This will save time waiting for `stack` to check every executable before compiling what's changed. Each package needs a separate directory tree and a separate `.cabal` file. 

## Creating the first package
Then create the basic `stack` project with it. This will create a new directory. Note that this new directory name can't have a hyphen-delimited word that's just digits, so the initial project will have to be `adventofcode1701`

```
stack new adventofcode1701 simple
```

This project will be demoted to being a package, but one that will hold the overall project. 

Then create the top-level `stack.yaml` file to hold the overall project information.

```
stack init
```

Modify this top-level `stack.yaml` file as needed, such as adding the `ghc-options` stanza. You can then delete `adventofcode1701/stack.yaml`. 

## Creating subsequent packages

Each package needs a separate directory tree and a separate `.cabal` file. 

To work on a project, `cd` into that project's directory.

Compile with
```
stack build
```
or 
```
stack build adventofcode1701
```

Run with
```
stack exec advent01
```

Run interactively with
```
stack ghci adventofcode1701:exe:advent01
```

To profile, use 
```
stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts" adventofcode1601
```
then run with
```
stack exec -- advent01 +RTS -p -hy
```

# Readme

Build this readme file wth
```
pandoc -s README.md > README.html
```

(Using the [Modest style](https://github.com/markdowncss/modest).)
