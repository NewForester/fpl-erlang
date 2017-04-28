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

    <title>Learn You Some Erlang Notes: Common Data Structures</title>
</head>

<body>

# Learn You Some Erlang

## A Short Visit to Common Data Structures

### Won't be long, promised!

Earlier sections introduced tuples and lists, the two basic data structures of Erlang.
This section is about introducing some of the other data structures.

Apart from the first subsection on records, all introduce not new Erlang data types,
but Erlang libraries that provide the means of handling specific data type abstractions.
I think part of the idea is to make the reader aware that there are often alternatives.

### Records

Records, as in C structures, are a feature of imperative languages before object re-orientation.
They do not sit well in functional languages and Erlang is no exception.
They are an afterthought, a hack, syntactic sugar that can choke you.

Nonetheless, they can be useful for small data structures, especially when subject to change on a multi-programmer project.

Records generally implement CRUD semantics:  they hold state.
This is viewed as a bad thing in functional programming languages.

A (perhaps obvious, but perhaps not obvious) consequence of which is that updating a record does not update the record any more than a list operation alters the list.
Updating a record returns a modified copy of the record just as a list operation returns a modified copy of the list.

First of all, you have to declare the record type,
The declaration is a module attribute:

```erlang
    -record(robot, {
        name,
        type=industrial,
        hobbies,
        details=[]
    }).
```

Hmm ... looks vaguely like a tuple of names giving names to a tuple of values.
The declaration gives default values for only some of the tuple of values.
The others may be initialised to 'undefined'.

Here is an example of declaring a record instance:

```erlang
    first_robot() ->
        #robot{
            name="Mechatron",
            type=handmade,
            details=["Moved by a small man inside"]
            }.
```

However, when you run this:

```erlang
    1> c(records).
    {ok,records}
    2> records:first_robot().
    {robot,"Mechatron",handmade,undefined,
       ["Moved by a small man inside"]}
```

the record is printed a tuple because that is what it is.

There is hack in the shell (only I presume) that sugars over this:

```erlang
    1> rr(records).
    [robot]
    2> records:first_robot().
    #robot{
        name="Mechatron",
        type=handmade,
        hobbies=undefined,
        details=["Moved by a small man inside"]
        }
```

The shell has other record related functions:

    * rl/0 - list all record definitions
    * rl/1 - list some record definition(s)
    * rf/0 - forget all record definition(s)
    * rf/1 - forget some record definition(s)
    * rd/2 - define a record definition (instead of using -record(...).)
    * rp/1 - convert tuple to a record

The rd/1, rl/1 and rf/1 can take lists of record names, each of which can involves wildcards.

Accessing record fields is, according to the tutorial, ugly:

```erlang
    1> NestedBot = #robot{details=#robot{name="erNest"}}.
    #robot{name = undefined,type = industrial,
       hobbies = undefined,
       details = #robot{name = "erNest",type = industrial,
                        hobbies = undefined,details = []}}
    2> (NestedBot#robot.details)#robot.name.
    "erNest"
```

Note: Erlang 14 and later do not require the parentheses but earlier releases do.

Records can be used in function heads to pattern match and in guards:

```erlang
    -record(user, {id, name, group, age}).

    %% use pattern matching to filter
    admin_panel(#user{name=Name, group=admin}) ->
        Name ++ " is allowed!";
    admin_panel(#user{name=Name}) ->
        Name ++ " is not allowed".

    %% can extend user without problem
    adult_section(U = #user{}) when U#user.age >= 18 ->
        %% Show stuff that can't be written in such a text
        allowed;
    adult_section(_) ->
        %% redirect to sesame street site
        forbidden.```
```

Note that the illustration above shows clearly that a function can act on a record without knowing how many fields there are in the record or their order.
This is something you cannot do with a tuple.

Records can be updated:

```erlang
    repairman(Rob) ->
        Details = Rob#robot.details,
        NewRob = Rob#robot{details=["Repaired by repairman"|Details]},
        {repaired, NewRob}.
```

and then:

```erlang
    1> c(records).
        {ok,records}
    2> records:repairman(#robot{name="Ulbert", hobbies=["trying to have feelings"]}).
    {repaired,#robot{name = "Ulbert",type = industrial,
                 hobbies = ["trying to have feelings"],
                 details = ["Repaired by repairman"]}}
```

but underneath is a call to `erlang:setelement/3` which return an updated copy of the original tuple.

#### A note on header files ...

Erlang does support header files.
However, the tutorial distrusts them:  they are a mechanism by which other languages share data but in functional programming languages, the whole idea is not to share data.

The syntax is:

```Erlang
  -include("records.hrl")
```

It may be used to share record definitions between modules.
While this is common practice, the tutorial views this as bad practice:
a module should encapsulate all data structures is requires and present only a simple function call interface.

### Key-Value Stores

Key-value stores are build into many languages (i.e. the dict of Python) but not, historically, Erlang (or functional languages in general).

Instead there are a number of libraries available.
Four are introduced below but there are even more specialised libraries for use with concurrency (such as the `mnesia` database).

#### Property Lists

This is a list of {Key,Value} tuples.
This is a very simple and loose definition.

The `proplists` library provide some functions that help work with such property lists but it is far from complete (no add or update functions).

Use it for small lists.
An example might be for configuration options read from an ini file.

#### Ordered Dictionaries

The `orddict` library is more formal and complete.
The values of Keys are unique and the the dictionary is ordered for faster lookups.

These are good for CRUD semantics on small data sets (up to 75 keys or so).

#### Dictionaries

The `dict` library provides the same interface and is a better choice for larger data sets.
It also has useful higher level functions such as dict:map/2 and dict:fold/2.

#### General Balanced Trees

The `gb-trees` library provides yet more functions that provide finer control over the data set.

This library provides two modes 'naive' and 'smart'.
The latter is for when you understand your structure inside and out.

One difference between the two is the 'naive' mode chooses when to rebalance the tree while 'smart' mode leaves it to the programmer to decide.

This library has a gb_trees:map/2 function but no fold function.
Instead there are iterators so that you can implement your own recursive fold functions.

It would seem that this library is slower in the general retrieval case but faster for operations that alter the tree.

#### Postscript Maps

With Erlang 17, the language has a native key-value data type.
See [Postscript Maps](http://learnyousomeerlang.com/maps).

This should be the defacto replacement for `dict` library.


### Arrays

These are not a comfortable data structure in Erlang.

There is an `array` library but it cannot offer constant time insertion or lookup.

It does, however, have a fold function that will skip undefined slots.

Erlang is not good with arrays and matrices.
Erlang programs often use `ports` to export heavy lifting work to other languages.


### A Set of Sets

There is no 'best' way to implement sets in a language like Erlang so there are four libraries to choose from.

Sadly, they are not flexible enough to be compatible.  `sets` uses =:=, while the others use ==.
Thus `sets` views 2 and 2.0 as distinct keys, which the others do not.

#### Ordered Sets

These are implemented as a sorted lists in the `ordsets` module.

They offers the simplest and most readable representation but are the slowest.
Good for small sets.

#### Sets

The `sets` library uses a structure very similar to that used by the `dict` library.

They have the same interface as ordered sets but scale better.
They are good at retrieval.

#### General Balance Sets

The `gb_sets` library is built on the `gb_trees` library.

They are faster for operations that change the set, offer a compatible interface but also
extra functions including 'naive' and 'smart' modes, iterators and quick access to first and last.

#### Set of Sets

The `sofs` library implements something much closer to the mathematical concept of sets than the others
(which implement groups of unique elements).
They sound quite specialised.


### Directed Graphs

These are supported by the `digraph` and `digraph_utils` modules.

These are a mathematical data structure that I guess has applications in networking,
which might explain why they are mentioned when many other languages have no direct or indirect support for them.

In mathematics they are closely related to sets and the `sofs` module provides some support for conversion between sets and directed graphs.


### Queues

Erlang lists are implicitly LIFO data structures.
It is cheap to add to and remove from the head of a list.
It is expensive to add to and remove from the end of a list because there is no tail pointer.

Tail recursion implicitly reverses a list and most tail recursive functions have a call to lists:reverse/1 as their final action.

When you need FIFO operation, cannot reverse a list all in one go or the list is long lived but its contents are not, you may be better off using the `queue` module.

This module has one API that, mentally, can be divided into three:

  * original API - the basic queue concept;
  * extended API - with nice-to-have functions;
  * Okasaki API - a purely functional interface but a bit weird.


### End of the short visit

The section ends with encouragement to explore
the Erlang (http://www.erlang.org/doc/apps/stdlib/index.html)[standard] and
(http://www.erlang.org/doc/applications.html)[extended] libraries for oneself.

Although we appear to be only a third of the way through, this completes the examination of Erlang the functional programming language.
The tutorial suggests that folk are much more interested in the Erlang the concurrent language.

I hope this means that the sections in the rest of the tutorial are smaller and more focused.

</body>
</html>
