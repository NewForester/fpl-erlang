<!DOCTYPE html>
<html lang="en-GB">
    <!-- erlang notes by NewForester is licensed under a Creative Commons Attribution-ShareAlike 4.0 International Licence. -->
<head>
    <meta charset="UTF-8" />
    <meta name="description" content="Notes on the Erlang programming language made while learning a bit about Functional Programming" />
    <meta name="keywords" content="Erlang" />
    <meta name="author" content="NewForester" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <link rel="stylesheet" href="../styles/style-sheet.css" />

    <title>Learn You Some Erlang Notes: Types</title>
</head>

<body>

# Learn You Some Erlang

## Types (or lack thereof)

Erlang uses dynamic, not static, typing.
This is disconcerting for those who have come to rely on the latter to build safe code.

The tutorial takes the view the weakly typed languages convert between types implicitly (with gay abandon)
while strongly typed lanaguage require the programmer convert types explicitly when they feel the need.


### Dynamite Strong Types

In Erlang code, 'variables' and 'functions' are written without types.

It is clear from the pattern {X,Y} that a tuple of two is expected but that tuple could be a pair of atoms,
an atom and a number or even another tuple and a list.

Only at run-time will it matter if what is supplied is not compatible with what is expected.
This is roughly what is mean by dynamic typing - bad types can only be caught at run-time and not earlier
(at compile-time).

There is the general view, with which I concur, that catching errors early is good as it is cheaper to fix them sooner rather than later.
The idea behind statically typed langauges is that the compiler can catch most errors before the code is ever run.
Proponents of such languages have a point but it is kind of narrow minded to suggest all errors of any importance are type typos.

The Ericsson AXD 301 ATM switch comprises over 1 million lines of code and yet is reported to have 99.9999999% availability.

There is no claim that the switch is bug free but the point here is that it is extremely robust.
Individual parts of the switch may crash but the whole switch never crashes - it recovers.
This robustness in the result of a lot of other design choice:  the dynamic or static nature of the programming language is irrelevant.

To put it another way:  there are those than think that using a safer programming language makes them safer programmers.


### Type Conversions

Erlang is strongly typed:  it has no implicit type conversions (other than integer to float).

Conversion is by means of casting and casting is by means of function calls of the form `<type>_to_<type>`.

These functions are all implemented as BIFs (many could not be implemented in Erlang).
All are in the Erlang module and, it seems, all require the module name:

```erlang
    1> erlang:list_to_integer("54").
    54
    2> erlang:integer_to_list(54).
    "54"
```

There are going on 30 such conversions (so not all possibilities are supported).

```erlang
    atom_to_binary/2            atom_to_list/1
    binary_to_atom/2            binary_to_existing_atom/2
    binary_to_list/1            binary_to_term/1
    bitstring_to_list/1
    float_to_list/1             fun_to_list/1
    integer_to_list/1           integer_to_list/2
    iolist_to_binary/1          iolist_to_atom/1
    list_to_atom/1              list_to_binary/1
    list_to_bitstring/1         list_to_existing_atom/1
    list_to_float/1             list_to_integer/2
    list_to_pid/1               list_to_tuple/1
    pid_to_list/1               port_to_list/1
    ref_to_list/1
    term_to_binary/1            term_to_binary/2
    tuple_to_list/1
```

At a guess, for those functions that take two parameters, the second parameters allows one or more options
to be passed to the conversion.  As a list of atoms, of course.

### To Guard a Data Type

Erlang tuples and lists and other data types give visual clues that allow type specfic pattern matching.
Guards were introduced to constrain values.
The `test type` BIFs allow guards to test the type of arguments so they can also constrain types.

The full list is

```erlang
    is_atom/1           is_binary/1
    is_bitstring/1      is_boolean/1        is_builtin/3
    is_float/1          is_function/1       is_function/2
    is_integer/1        is_list/1           is_number/1
    is_pid/1            is_port/1           is_record/2
    is_record/3         is_reference/1      is_tuple/1
```

The other BIFs allowed in guards are:

```erlang
    abs/1               bit_size/1              byte_size/1
    element/2           float/1                 hd(List)/1
    length/1            node/0                  node/1
    round/1             self/0                  size/1
    tl/1                trunc/1                 tuple_size/1
```

#### A note about testing for the right cases ...

The reason Erlang provides a whole bunch of test type functions instead of a single type_of/1 -> Type function
is that Erlang seeks to encourage coding for what you do know, not what you don't.

Code for the cases you know and expect and allow Erlang to throw exceptions for everything else.
This is an interesting take on defensive programming.

#### A note about Erlang data structures ...

Data structues in Erlang may seem limited at first sight but they are suprisingly powerful:

```erlang
    {person, {name, <<"Fred T-H">>},
        {qualities, ["handsome", "smart", "honest", "objective"]},
        {faults, ["liar"]},
        {skills, ["programming", "bass guitar", "underwater breakdancing"]}}.
```

### For Type Junkies

Erlang was developed with dynamic typing simply because the original team were used to such languages.

There have been several attempts to add static typing to Erlang but without success.
Part of the reason is that processes and messages are fundamental Erlang types.

The folk that wrote the Haskell compiler failed.
The HiPE project also failed but it did produce Dialyzer, a staic analysis tool still in use today.

The typing system Dialyzer uses is called 'success typing'.
It does not seek to infer the type of every expression but does guarantee that any type it does infer is correct and
that any type errors it finds really are errors.

Take the `and` function of Erlang.
All that can be said about it is that it returns true or false and that for it to return true,
both parameters must also be true.
You cannot even say that it takes two Booleans and returns a third.

Another tool, TypEr can be used to generate annotations for functions but it is unclear why you would want to.

</body>
</html>
