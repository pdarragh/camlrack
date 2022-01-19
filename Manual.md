# Camlrack Manual

This document provides the complete manual for using
[Camlrack](https://github.com/pdarragh/camlrack), an S-Expression
pattern-matching library for OCaml.


## Table of Contents

  *


## Getting Started

Because Camlrack is still under relatively active development, it may be useful
to install the library from source. To do this, clone [the repository from
GitHub](https://github.com/pdarragh/camlrack) to your local computer and install
it using [Dune](https://dune.build):

```
$ cd ~/some/coding/directory/
$ git clone https://github.com/pdarragh/camlrack.git
$ cd camlrack
$ dune install
```

Once complete, you can add Camlrack as a dependency for any project built with
Dune by using the `libraries` directive:

```
(library
 (public_name my_cool_library)
 (libraries camlrack)
 ...)
```


## Using Camlrack

Camlrack can either be used as a traditional library or as a PPX rewriter,
though using both together is likely to be the most useful option.
