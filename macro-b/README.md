# macro-blog

### Intro

This repository contains Racket macros by either Michael or me. 

The reason this repo is named `macro-blog` is because the code in this repository is primarily meant for humans to read and understand (and incidentally for computers to execute).

The code can be found in the `.rkt` files in the subfolders, but I have also included the code in `README.md`  possibly accompanying a picture as an explanation. 

### Macros

- The **`first-order`** macros make it possible to have compile-time guarantee that a function application's arguments don't exceed the number of parameters that the function was defined with. 

  - [`first-order1`](https://github.com/atharvashukla/macro-blog/tree/master/first-order1): `define-first-order`defines calls a sub-macro, `enforce-first-order`, which enforces the argument constraint passed as a param to it.
  - [`first-order2`](https://github.com/atharvashukla/macro-blog/tree/master/first-order2): `define-first-order` makes a macro that converts all function occurrences to a compile time struct, which `apply-first-order` extracts from.

- The **[`struct-info.rkt`](https://github.com/atharvashukla/macro-blog/tree/master/struct-info)** file has a `get` macro. You can supply a struct instance, field, and the struct-name and use get as a selector on the instance. a wrong selector would lead to a compile time error.  
