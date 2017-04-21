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

    <title>Learn You Some Erlang Notes: Syntax In Functions</title>
</head>

<body>

# Learn You Some Erlang

## Syntax In Functions

This section presents three ways of laying out conditional code.

In some circumstances you can use any one of them.
There are no hard and fast guidelines as to which is to be used when.

Generally speaking, Pattern Matching with function heads is to be preferred over Case ... Of,
which is to be preferred over the If ... End.


### Pattern Matching (Function Heads)

In Erlang functions, `function heads` avoid a lot of if ... else boiler plate.

```erlang
    greet(male, Name) ->
        io:format("Hello, Mr. ~s!", [Name]);
    greet(female, Name) ->
        io:format("Hello, Mrs. ~s!", [Name]);
    greet(_, Name) ->
        io:format("Hello, ~s!", [Name]).
```

This defines `greet/2`, a function with three clauses.

Whenever the function is invoked and the first argument is the atom 'male',
the first clause matches the parameters and the first 'io:format' statement is executed and its result returned.

Whenever the function is invoked and the first argument is the atom 'female',
the second clause matches the parameters and the second 'io:format' statement is executed and its result returned.

Otherwise the third clause matches (`_` always matches) and the third statement is executed and its result returned.

Each clause is terminated with a semi-colon (`;`) except the last, which is terminated with a full stop (`.`) to indicate the end of the function.

Pattern matching can recognised tuples:

```erlang
    valid_time({Date = {Y,M,D}, Time = {H,Min,S}}) ->
        io:format("The Date tuple (~p) says today is: ~p/~p/~p,~n",[Date,Y,M,D]),
        io:format("The time tuple (~p) indicates: ~p:~p:~p.~n", [Time,H,Min,S]);
    valid_time(_) ->
        io:format("Stop feeding me wrong data!~n").
```

but cannot, on its own, check the validity of the tuple's contents.

#### A note on `io:format` ...

This is the Erlang analogue to the printf() of C but it is not entirely equivalent.

Here `~s` demands the substitution of a string (list).
Earlier we saw `~n` substitute `\n`.
Widely used is `~p`, which does a pretty print (of tuples and lists and stuff).
More later.


### Guards, Guards !

In Erlang, `guards` add constraints to pattern matching much the same way as the constraints seen earlier in list comprehensions:

```erlang
    right_age(X) when X >= 16, X =< 104 ->
        true;
    right_age(_) ->
        false.
```

or:

```erlang
    wrong_age(X) when X < 16; X > 104 ->
        true;
    wrong_age(_) ->
        false.
```

Note the first example separates the guards with `,`, which may be read as 'and also'
while the second example separates the guards with `;`, which may be read as 'or else'.

However, they are not the same as the reserved words `andalso` and `orelse`.
These do not catch an exception, in which case the guard fails whereas `,` and `;` will catch the exception
and treat it as if the condition were false.

Guards cannot have side-effects.
To ensure this, Erlang restricts what is allowed in guards.
Most BIFS, yes, user defined functions, no.

This means guards may comprise `andalso` and `orelse` (and these may be 'nested':  not so `,` and `;`).


### In Case ... of

In Erlang, a function header (with or without guards) is very much the same as a 'case' or 'switch' block in other languages.
Erlang provides its own version in the `case ... of` construct.

This snippet will construct a list of unique elements:

```erlang
insert(X,[]) ->
    [X];
insert(X,Set) ->
    case lists:member(X,Set) of
        true  -> Set;
        false -> [X|Set]
    end.
```

Pattern matching is used to distinguish the empty and non-empty list case.
The Case ... Of contruction is used to distingish the present and not present cases.

The above might not be good Erlang but I agree that you probably cannot refactor it to use function heads without introducing a second function.

Here is an example that could:

```erlang
    beach(Temperature) ->
        case Temperature of
            {celsius, N} when N >= 20, N =< 45 ->
                'favorable';
            {kelvin, N} when N >= 293, N =< 318 ->
                'scientifically favorable';
            {fahrenheit, N} when N >= 68, N =< 113 ->
                'favorable in the US';
            _ ->
                'avoid beach'
    end.
```

Here is the example using function heads:

```erlang
    beachf({celsius, N}) when N >= 20, N =< 45 ->
        'favorable';
    beachf({kelvin, N}) when N >= 293, N =< 318 ->
        'scientifically favorable';
    beachf({fahrenheit, N}) when N >= 68, N =< 113 ->
        'favorable in the US';
    beachf(_) ->
        'avoid beach'.
```

### What the If !?

There is an `if ... end` statement in Erlang.
The view in the tutorial is that this is a valid construct with a confusing name because it is not like the 'if' of other languages.

In Erlang, an If ... End statement also uses `guard patterns`.
See the [what_the_if](../tut-lyse/what_the_if.erl) modules for the examples.

The simple form of the statement has one clause:

```erlang
    if Guard -> Expression end
```

There may be many clauses providing something similar to 'elif' in other languages:

```erlang
    if Guard -> Expression;
       Guard -> Expression;
       ...
    end
```

Everything in Erlang must return something so often the final guard is `true` which provides someting similar to 'else' in other languages:

```erlang
    if Guard -> Expression
       true  -> Expression
    end
```

#### A note on there being no else ...

In an imperative language, If ... Else is about doing one thing or another such as calling one of two procedures (possibly with side-effects).

In Erlang, If ... End is about returning a result perhaps by calling one of two functions (without side-effects).

In an imperative language, the Else clause is optional - no harm done when the If evaluates to false.

In Erlang, the absence of True clause will mean an exception is thrown should the If clause evaluate to false.

In Erlang, the If ... End statement may have many clauses.
A final True clause is much more like the 'default' of the switch statement in C.

The `true` clause is thus a catch-all and there are grounds for viewing these as bad.

In C, the catch-all should print an 'unexpected value' (error) message.
In Python, the catch-all should raise an exeception.
In Erlang, the exeception is thrown for you.

In all cases, the programmer should add good code to handle the unexpected value.
Anything else is GIGO.

Thus in Erlang:

```erlang
    if X > Y -> a()
     ; X < Y -> b()
     ; X ==Y -> c()
    end
```

may be considered better than:

```erlang
    if X > Y -> a()
     ; X < Y -> b()
     ; true  -> c()
    end
```

### Which to Use ?

There is no clear cut answer to such a question.
In some circumstances, one of three methods is a clear winner but in many cases you could equally well use any.
There is really is no performance penalty.

The function heads requires a function, use this method if there is an obvious function to implement.
Consider using one of the others when you would otherwise be adding a function for no good reason.

The Case ... Of can be used on more than one variable (eg `case {A,B} of`) but when there is more than one
variable, most Erlang progrmmers will opt for the function heads approach.

The If ... End was added to the language for when there are no patterns, only guards.
Use them this way.

</body>
</html>
