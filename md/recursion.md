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

    <title>Learn You Some Erlang Notes: Recursion</title>
</head>

<body>

# Learn You Some Erlang

## Recursion

Functional programming languages lack loop constructs:  they use recursion instead.

See also [Recursion](../tut-lyse/recursion.erl) for code examples.

### Hello Recursion!

In Erlang, functions do not loop but they call themselves - this is known as recursion.

The recursion notation is derived from mathematical concepts, just as the 'invariant' variables are.

```erlang
    fac(N) when N == 0 -> 1;
    fac(N) when N > 0  -> N*fac(N-1).
```

Implements factorial(n) in only two statements.
The first represents the `base case` that, at run-time, terminates the recursion
(no infinite ~~loop~~, sorry, recursion, please).
The second embodies the 'by induction' part, where factorial(n) is a function of factorial(n-1).

It is difficult to imagine an simpler, more expressive, notation.


### Length

Using recursion, the length of the list might be determined thus:

```erlang
    len([]) -> 0;
    len([_|T]) -> 1 + len(T).
```

There is one snag with this kind of recursion:
each level of recursion has a context that must be kept while the lower levels are evaluated.

The number of such contexts and thus the memory required, is in direct proportion to the length of the list.
Not good, especially for long lists.


### Length of Tail Recursion

Tail recursion is an alternative way of expressing recursion that, in principle, does not require the keeping
of contexts and thus, in practice, allows the compiler to generate code that requires a fixed amount of
memory.  Good, especially for long lists.

The technique relies on the recursive call being the last thing the recursive function does and the call being 'alone'.
This boils down to the last statement of the function being the recursive call and nothing but the recursive call.

The context is not kept but is passed down as an extra parameter called the accumulator.
This means a tail recursive algorithm in Erlang is a pair of functions:

```erlang
    tail_fac(N) -> tail_fac(N,1).

    tail_fac(0,Acc) -> Acc;
    tail_fac(N,Acc) when N > 0 -> tail_fac(N-1,N*Acc).
```

### More Recursive Functions

This section covers more recursive functions to help reinforce the message.
See also [Recursion](../tut-lyse/recursion.erl) for the code examples.

The tutorial suggests that tail recursion is very similar to the while loops of imperative languages.

It also illustrates that the accumulator can be anything, not just a number.
In particular, it can be another list.

Given that (in functional programming languages) adding an element to the front of a list is much cheaper
that adding it to the end, tail recursive routines generate their results backwards and the result must be reversed before the final return..

An example of how to reverse a list is given but the tutorial points out that, in practice,
one should use the library routine `lists:reverse/1`.

The sublist example shows that there may be two base cases:  have the first N elements and there are not N elements to have.

The zip example shows that sometimes the number of base cases is a design decision.

Finally, there is the admission that tail recursion is good because the compiler recognises it and
eliminates the current stack frame before making the recursive call.
This is called Tail Call Optimisation (TCO) and is a special case of Last Call Optimisation (LCO).

### Quick, Sort!

An implementation of the quick sort algorithm is the classic example of the expressive power of recursion.

The tutorial presents a simple version where the first element is taken to a pivot.
The list is then divided into two:  those less than and those greater than the pivot.
The process is then repeated on the two halves and so on until each half comprises just one element.
The halves are then concatenated together.

Smarter algorithms try to be smarter about choosing the pivot.

Two functions are required.  One to partition the list and another to drive the recursion and concatenate the results.

The second function may be expressed as:

```erlang
    quicksort([]) -> [];
    quicksoft([Pivot|Rest]) ->
        {Smaller, Larger} = partition(Pivot,Rest,[],[])
        quicksort(Smaller)++[Pivot]++quicksort(Larger).
```

The first as:

```erlang
    partition(_,[],Smaller,Larger) -> {Smaller, Larger}
    partition(Pivot,[H|T],Smaller,Larger) ->
        if H =< Pivot -> partition(Pivot,T,[H|Smaller],Larger);
           H  > Pivot -> partition(Pivot,T,Smaller,[H|Larger]);
        end.
```

An alternative, that may be a little easier on the eyes uses list comprehensions:

```elang
    lc_quicksort([]-> [];
    lc_quicksort([Pivot|Rest]) ->
        lc_quicksort([Smaller || Smaller <- Rest, Smaller =< Pivot])
        ++ Pivot ++
        lc_quicksort([Larger || Larger <- Rest, Larger > Pivot).
```

The downside of this algorithm is two comprehensions means each (partial) list is traversed twice.

What tutorials seldom mention is that these algorithms are not very efficient.
In the real world, you would use `lists:sort/1`.


### More Than Lists

Recursion is best demonstrated using lists, the simplest of recursive data types and one built into the language.

However, lest the impression be given that lists are the only recursive data structure, the tutorial considers a binary tree example.

The tree consists of nodes, each of which has two children.  At the tips of the tree are empty nodes:

```erlang
    {node, 'nil'}                               % the empty node - a tagged tuple
    {node, {Key, Value, Smaller, Larger}}       % a not empty mode
```

There is a Key and its Value and two sub-trees, Smaller and Larger.
The Key is unique.
The Key is guaranteed (by the algorithm that inserts nodes) to be greater than the Keys
of all nodes in the Smaller subtree and less than all the Keys in the Greater subtree.

It is a way of storing an ordered list so that the time taken to find an arbitrary Key is O(log<sub>n</sub>).

As it happens, the quicksort algorithm effectively builds a binary tree on the fly.
I guess its execution time is also O(log<sub>n</sub>) and I suspect that using a real binary tree might use less memory except for small n.

See [Binary Tree](../tut-lyse/tree.erl) for the code examples.

The insert/3 function returns a new tree.
This may appear inefficient but the COW strategies within the virtual machines may mean that it is not so.

The lookup/2 function has two base cases:  key found and key not found.
To distinguish between these, one returns {ok, Value}, the other {undefined}.
Wrapping successful cases this way is a common idiom in Erlang.

The implementation is sufficient to prove the point.
In the real world, the tree modules would require at least a function to delete a node and another to rebalance the tree.

There is also the `gb_trees` module in the Erlang library that should be used in preference to home brew implementations.


### Thinking Recursively

Recursive programming of functional languages is more declarative than the corresponding loop programming of imperative languages.

Coupled with pattern matching, it can be the optimal solution to the problem of expressing algorithms so they are concise and easy to understand.

The tutorial concludes with a discussion of whether tail recursion is worth the effort.
It concludes that it is but only because you don't have to:
most recursive algorithms have already be written and are available in the Erlang library.

</body>
</html>
