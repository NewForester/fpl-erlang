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

    <title>Learn You Some Erlang Notes: Higher Order Functions</title>
</head>

<body>

# Learn You Some Erlang

## Higher Order Functions

### Let's get functional

An important part of Erlang and all functional languages is the ability to pass
a function as a parameter to another function (and to return a function from a function).

Functions that take or return other functions are named `higher order functions`.
They are another concept rooted in mathematics.
The concept is call lambda calculus.

In lambda calculus, everything is a function, even numbers are functions.

See `one/1`, `two/1` and `add/2` in [Higher Order Functions](../tut_lyse.erl).

Some new, not very pretty, notation has been added.  To pass a function as a parameter, you have to write it:
`fun Module:Function/Arity`.

To illustrate the advantages of passing functions as functions consider:

```erlang
    increment([]) -> [];
    increment([H|T]) -> [H+1|increment(T)].

    decrement([]) -> [];
    decrement([H|T]) -> [H-1|decrement(T)].
```

These functions are very similar.
The process-a-list part is the same, only the operation performed on each element of the list is different.

```elang
    map(_,[]) -> [];
    map(F,[H|T]) -> [F(H)|map(F,T)].

    incr(X) -> X+1.
    decr(X) -> X-1.
```

Separates the two nicely but now we have three, not two, functions and four, not two, lines of code.
We need two 'anonymous' functions that can be used without ever being declared.


### Anonymous functions

In Erlang, anonymous functions are called `funs` because they too use the `fun` keyword:'

```erlang
    fun (Args1) ->
            Expression1, Exp2, ...., ExpN;
        ...
        (ArgsN)->
            Expression1, Exp2, ...., ExpN
    end
```

The simplest case possible is something like:

```erlang
    1> Fn = fun() -> ok end.
    #Fun<...>
    2> Fn().
    ok
```

but it rather defeats the point.
Most useful, here is the increment/decrement again but without the silly one liners:

```erlang
    1> hhfuns:map(fun(X) -> X + 1 end, L).
    [2,3,4,5,6]
    2> hhfuns:map(fun(X) -> X - 1 end, L).
    [0,1,2,3,4]
```

The loop has been abstracted away into `map/2` and the programmer can focus on what is to be done rather than on how to do it.

#### Closures

To introduce and understand `closures` consider the following:

```erlang
    PrepareAlarm = fun(Room) ->
        io:format("Alarm set in ~s.~n",[Room]),
        fun() -> io:format("Alarm tripped in ~s! Call Batman!~n",[Room]) end
        end.
```

The invariant PrepareAlarm has been bound an anonymous function.
Next:

```erlang
    1> AlarmReady = PrepareAlarm("bathroom").
    Alarm set in bathroom.
```

This runs the anonymous function, which prints the message and returns another anonymous function, which is bound to AlarmReady.

```erlang
    2> AlarmReady()
    Alarm tripped in bathroom! Call Batman!
```

How does AlarmReady know the alarm is in the bathroom ?

The second anonymous function is defined in terms of Room, which is within the scope of the first anonymous function.
Inheritance of scope is quite normal in programming languages that have scope.

At the time the second anonymous function is instantiated, Room had the value 'bathroom'.
Room with the value 'bathroom' becomes part of the scope of the second anonymous function and
this remains so even after the first anonymous function has returned and it's Room has gone out of scope.

This is seems perfectly normal in the example above but it is not how many languages behave.
This behaviour is typical (even essential) to functional programming languages and is called `closure`.

Other descriptions of closure have confused but Batman makes it look so simple.

The tutorial suggest closures are used to carry state, which is indeed what is happening.
Be careful who you say that to:  functional programmers do not believe in state.

#### Anonymous Function Limitations

One of the limitations of anonymous functions in Erlang was they could not call themselves,
so they could not be used to implement recursion.
From Erlang 17 onwards this lacuna has been fixed.
Anonymous function can have a name limited to their own scope so that they can call themselves:

```erlang
    fun Name1(Args1) ->
            Expression1, Exp2, ...., ExpN;
        ...
        NameN(ArgsN)->
            Expression1, Exp2, ...., ExpN
    end
```


### Maps, filters, folds and more

Earlier the map/2 abstraction was introduced.
It can be used to apply any function to each element of a list.
It hides the details of how this is done and all the programmer needs to do it supply the function.

There are other similar abstractions.  Filter and fold are two of the most powerful.
See `filter/2` and `fold/2` in [Higher Order Functions](../tut_lyse.erl) for examples.

Filter takes a function called a predicate, applies the predicate to each element of the list and returns only those elements for which the predicate returns true.

Fold takes a function and applies it to each element of the list and the accumulator.
This reduces the list to the final accumulator value.
The choice here is the initial value of the accumulator.

Fold can be used to find the minimum and maximum of the list (using head as the initial value of the accumulator)
and sum over the elements of a list (using 0 as the initial value of the accumulator).

There are no constraints on what the accumulator is.
Reverse, map and filter can all be implemented in terms of fold if the accumulator is a list.

```erlang
    fold(_, Start, []) -> Start;
    fold(F, Start, [H|T]) -> fold(F, F(H,Start), T).

    reverse(L) ->
        fold(fun(X,Acc) -> [X|Acc] end, [], L).

    map2(F,L) ->
        reverse(fold(fun(X,Acc) -> [F(X)|Acc] end, [], L)).

    filter2(Pred, L) ->
        F = fun(X,Acc) ->
            case Pred(X) of
                true  -> [X|Acc];
                false -> Acc
            end
        end,
        reverse(fold(F, [], L)).
```

Erlang provides many abstractions in the list library so in practice you study and use those instead of
writing your own recursive list functions.
See [documentation on lists](http://erldocs.com/18.0/stdlib/lists.html).

</body>
</html>
