WARNING : numcaml is in early prototype stage. Any contribution/feedback is welcome!

The goal of numcaml is to be able to deal with numerical computations more easily in Objective Caml.
More precisly, it consists of 3 parts :

* a camlp4 syntax extension;
* a complete and efficient library; and
* a collection of visualisation tools.

Numcaml is mainly inspired from the great [numpy](numpy.scipy.org/) library for python.
The static types of Objective Caml (and the lack of automatic upcasting) makes it difficult to port
numpy directly as a library. For example :

    1 + 2.3 

is valid in python because the integer `1` can be upcasted to the float `1.`, and `+` looks dynamically
at the type of its arguments to know which code to execute. In Objective Caml, the expression is
rejected because `+` is a function which *must* takes two integers as arguments.

The syntax extension of numcaml let you write :

    $(1 + 2.3)

inside your Objective Caml programm. This expression is translated, at pre-processing stage, into :

    Math.add (Math.Int 1) (Math.Float 2.3)

and `Math.add` will do the same dynamic dispatch as python is doing for `+`.

You can see more complete examples of using the numcaml library in the `tests/` directory.

 