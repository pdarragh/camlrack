# Camlrack Manual

This document provides the complete manual for using
[Camlrack](https://github.com/pdarragh/camlrack), an S-Expression
pattern-matching library for OCaml.

**Note on platforms.** This guide is written assuming the reader is using a
Unix-like environment. If you are on Windows and not using the Windows Subsystem
for Linux (WSL), I cannot help you because I do not have a Windows machine to do
any testing. I use macOS, but I believe the instructions should be sufficiently
platform-agnostic to work for Linux or WSL.


## Table of Contents

  * [Installation](#installation)
      * [From OPAM](#from-opam)
      * [From Source](#from-source)


## Installation

Camlrack requires OCaml 4.12 or greater. You can install OCaml by following
[these instructions](https://ocaml.org/learn/tutorials/up_and_running.html).


### From OPAM

You can install Camlrack and its PPX extensions from OPAM by doing:

```text
$ opam install camlrack ppx_camlrack
```


### From Source

Camlrack's PPX extensions rely on the `ppxlib` library, so you'll need that
first. To install it, do:

```text
$ opam install ppxlib
```

clone [the Camlrack repository from
GitHub](https://github.com/pdarragh/camlrack) to your local computer and install
it using [Dune](https://dune.build):

```text
$ cd ~/some/coding/directory/
$ git clone https://github.com/pdarragh/camlrack.git
$ cd camlrack
$ dune build @install
$ dune install
```

Once complete, you can add Camlrack as a dependency for any project built with
Dune by using the `libraries` directive:

```dune
(library
 (public_name my_cool_library)
 ...
 (libraries camlrack)
 ...)
```

To use the PPX extensions, you can additionally use the `preprocess` directive:

```dune
(library
 (public_name my_cool_library)
 ...
 (libraries camlrack)
 (preprocess (pps ppx_camlrack))
 ...)
```

To update Camlrack when installed from source, you can pull the latest changes
from the git repository and re-install the library:

```text
$ cd ~/some/coding/directory/camlrack/
$ git pull
$ dune build @install
$ dune install
```


## Camlrack API

The full API for Camlrack is provided [in another file](Camlrack_API.md).


## Using Camlrack

First, we assume familiarity with
[S-Expressions](https://en.wikipedia.org/wiki/S-expression). S-Expressions are a
concise way to represent list-like data structures using only
