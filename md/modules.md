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

    <title>Learn You Some Erlang Notes: Modules</title>
</head>

<body>

# Learn You Some Erlang

## Modules

Erlang modules are not very complex so this section talks about other things you need to know as well.

Modules are imported and imported modules may import others.

The module name corresponds to a file name so the operating system determines what names modules may have:
the tutorial does not mention any other restrictions.


### What are Modules ?

In Erlang, as in some other languages, a `module` is a compilation unit.
All functions must be defined in a module.

Good modules group together related functions but this is good design, not just good Erlang.

Functions in an external module cannot simply be invoked by function name alone.
They may be invoked by module and function name:

```erlang
    Module:Function(Arguments)
```

That is, of course, not quite the end of the story.


### Module Declaration

In Erlang a `module` contains `attributes` and `functions`.

The `attributes` are meta-data that allow useful information about the module to be extracted without needing to examine the code.

There are many attributes and you may even define your own but one is required and a second is more or less compulsory.

```erlang
    -module(Name).
```

Is required else the Erlang compiler will not recognise the file as an Erlang module.
It must be the first attribute (and statement) of the module.

Name is an atom (so lower case is expected).
This is the module name used to invoke functions defined in the module from outside the module.

The source file name is Name.erl.

```erlang
    -export([Function/Arity, ...]).
```

declares the functions defined within the module that can be invoked from outside the module.
Without it the module has no functions that can be called and would therefore be of no practical use.

The `arity` declares the number of arguments a function expects:
functions with the same name but a different number of arguments are considered to be different functions.
This occurs often in Erlang modules.

#### A note on functions ...

The syntax of a function declaration is:

```erlang
    Name(Args) -> body.
```

Function names are as for atoms -they must begin with a lower case letter (or worse).

The final statement of a function is terminated with a full stop (`.`) so terminating the function.
Other statements are separated by a comma (`,`) and function clauses by a semi-colon (`;`).

There is no return statement but the function result is the result of the final statement.

All Erlang functions return something.
Many that have nothing to return, return the atom `ok`.

The Erlang shell will print the final result but not any intermediary ones.
It is not possible to declare functions in the Erlang shell:
they must be declared in module and then imported.

Here is a simple function declaration:

```erlang
    add(A,B) ->
        A + B.
```

#### A note on comments ...

In Erlang, comments are to the end of line and begin with `%`.

#### A note on Hello World ...

```erlang
    hello() ->
        io:format("Hello World!~n").
```

#### A note on the import attribute ...

```erlang
    -import(io, [format/1]).
```

will allow the `format` function to be invoked without the `io:` prefix.
This practice is usually discouraged (it is considered confusing) except for very commonly used functions, such as those defined in the `list` module.

#### A note on macros ...

These are very similar to those of C and are used most often to define manifest constants or short functions.

```erlang
    -define(PI,3.142)
    -define(add(A,B),A+B)
```

To use them in a function, prefix the name with a question mark (`?`).

```erlang
    ?PI * X / 180
    ?add(2,2)
```

### Compiling the Code

In Erlang, a module must be compiled to `byte code` (so that it can be used by the `virtual machine`) before any of its functions can be invoked.

If the source module is `module.erl`, the compiler will produce a byte code module named `module.beam`.

Here `beam` is the name of the virtual machine.  The name stands for "Bogdan/Björn's Erlang Abstract Machine".
There have been other abstract machines in the past but they are no longer used.

There are several ways of invoking the compiler.
From the OS shell:

```bash
    $ erlc <flags> module.erl
```

From an Erlang module or the Erlang shell:

```erlang
    compile:file(module)
```

From the Erlang shell:

```erlang
    c(module)
```

However, this last will only look for the module source file in the current directory.
To alleviate this inconvenience, the Erlang shell provides the function `cd/1`,
which takes a character string representation with '/' as a directory character.

The compiler accepts a whole bunch of compilation flags.
The most common are:

  * debug_info
  * {outdir,Dir}
  * export_all
  * {d,Macro}
  * {d,Macro,Value}

For command line use, precede each with a single dash (`-`) - Erlang is not GNU.

For shell use, there is a `c/2` that takes an array:

```erlang
    c(module, [debug_info, export_all]).
```

Likewise `compile:file/2`.

You can see now that `{d,Macro}` is a tuple and `export_all` is an atom.

It is also possible to define compiler flags using a module attribute:

```erlang
    -compile([debug_info, export_all]).
```

Note: on some platforms it is possible to compile to 'native' code.
Native code may be 20% faster than byte code.
However, the `beam` file is bigger as it contains both.


### More About Module Attributes

Module attributes are compile time.
The compiler creates a `module_info/0` function that returns a list of attributes and their values.
It also creates a `module_info/1` function that returns one attribute and its value.

An attribute and its value is a tuple but the value may itself be a list of tuples (and so on).

If you were to define your own attribute:

```erlang
    -author("NewForester")
```

Its value could be retrieved with ...

```erlang
    author() ->
        author(module_info(attributes)).
    author([{author,A}|_]) ->
        A;
    author([_|T]) ->
        author(T).
```

This section of the tutorial ends with a suggestion that circular dependencies 'between' modules be avoided.


</body>
</html>
