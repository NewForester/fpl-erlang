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

    <title>Learn You Some Erlang Notes: Errors and Exceptions</title>
</head>

<body>

# Learn You Some Erlang

## Errors and Exceptions

In Erlang, you are not encouraged to handle errors and exceptions.
The motto is let it crash (and fix the source code).

The section discusses the compilation and run-time errors you (the beginner) are likely to encounter because you will want to fix the source code to remove them.

It then discusses exceptions at length.
These are the main error mechanism in the functional subset of the language:
the mechanisms for the concurrent subset of the language will be covered with in another chapter.

It introduces the surprisingly complex exception mechanism, covers the older, simpler, one and ends with an example of a non-local return
were code deliberately throws an exception as a normal part of execution:  nothing exceptional there.


### A Compilation of Errors

Here are the most common compilation errors and what to do about them:

 1. Error: module name 'madule' does not match file name 'module'<br>
    Check for a typo in your `-module` attribute.

 2. Warning: function some_function/0 is unused<br>
    Check for a typo or omission or the wrong arity in your `-export` attribute(s).

 3. Error: function some_function/1 undefined<br>
    As above but also check the function compiled and is correctly terminated with a full-stop.

 4. Error: syntax error before: 'SomeCharacterOrWord'<br>
    Often unmatched parenthesis, braces or bracket or wrong punctuation character (comma where there should be a semi-colon)

 5. Error: syntax error before<br>
    Often means the previous line has not been terminated properly (e.g. missing full-stop at end of function).

 6. Warning: this expression will fail with a 'badarith' exception<br>
    An arithmetic expression is junk, probably due to a typo.

 7. Warning: variable 'Var' is unused<br>
    Could be a typo or a logic bug.  If the variable is genuinely unused, pre-pend or replace with '_'.

 8. Warning: a term is constructed, but never used<br>
    Some expression other than the final one is unbound.  Check your code.

 9. Error: head mismatch<br>
    Check the function header for typos; check the arity is the same for all clauses.

10. Warning: this clause cannot match because a previous clause at line 4 always matches<br>
    Check you haven't got a catch all where it should not be.

11. Error: variable 'A' unsafe in 'case'<br>
    Variable bound inside a case used outside of it.  Consider using MyVar = case ... of instead

Note:  for syntax errors, check the line and then the function before the one with the error.

The most common errors to date are missing a terminating full-stop, semi-colon or comma and
failing to match parentheses, braces and brackets.

Correcting one error may allow the compiler to flag others so the fix one-at-a-time approach works quite well.


### No, YOUR logic is wrong!

Logical errors belong to the programmer.
One of the most common is getting conditionals wrong so the code executes the wrong case.

Thorough testing of code before release is recommended and Erlang has tools, such as test frameworks and static analysers, that may help
but it is still very much up to the programmer to avoid logic errors and to debug the inevitable.

One of the nasty things about logic errors is they behave like rain water that seeps into the ground and bubbles up later miles away.
When the bug become apparent it is often very difficult to find out where it originated.

Hence the Erlang motto "let it crash, let is crash, let is crash" in the belief that such an approach will
tend to crash the code sooner rather than later making it easier to find the source of each problem.


### Run-time Errors

Run-time errors are destructive:  they crash the process.
Erlang has ways of dealing with these but you still need to understand what they mean in order to fix them.

 1. (function_clause)   no function clause matching<br>
    Check header clauses for a missing case or badly expressed pattern or guard.

 2. (case_clause)       no case clause matching<br>
    Check case ... of ... end for missing clause - avoid adding a catch-all.

 3. (if_clause)         no true branch found when evaluating an if expression<br>
    Check if ... end for missing clause - avoid adding the true catch-all.

 4. (badmatch)          no match of right hand side value<br>
    Check patterns - something not possible is being attempted like binding to a variable a second time or trying to match a list with a tuple.

 5. (badarg)            bad argument<br>
    Check arguments - indicates pattern and guard match but the argument is the wrong type

 6. (undef)             undefined function<br>
    Check for typo in the module and function names; check the number of parameters; check the function is exported; check the Erlang search path.

 7. (badarith)          bad argument in an arithmetic expression<br>
    Check operands - one of them is not a number or some other domain error like divide by zero

 8. (badfun)            bad function<br>
    Check arguments to higher order function - one that should be a function is not.

 9. (badarity)          interpreted function with arity nn called with mmm arguments<br>
    Check arguments to higher order function - there are too many or too few arguments.

10. (system_limit)      system_limit<br>
    Oops, this may crash the VM.  It usually means 'too many' of something so check for an infinite allocation loop or a resource leak.

The (atom) is what you need to catch if you want to handle the error.


### Raising Exceptions

Sometimes raising an exception is a good thing to do.
One example is to signal an impossible condition or one not (yet) handled.
Another is to perform a non-local return.

Erlang has three kinds of exceptions:  errors, exits and throws.

#### Errors

Calling `erlang:error(Reason)` provokes a run-time `error`.
The current process is terminated and a stack trace generated.

Errors signal conditions that calling code cannot be expected to handle and it is best to bail out and let the programmer reconsider.

The Reason is an atom of your choice:  it need not be, and perhaps should not be, one of those listed in the previous section.

#### Exits

An `exit` is produced by a call to exit/1 or exit/2.
The latter is concerned with concurrency.

Exits are similar to errors (once there were only exits) except no stack trace is produced: an exit is normal, there is nothing to debug.

In a concurrent environment, both generate a message that is sent to all processes listening for messages from the deceased process.
An error means they all get sent a potentially large message with a long stack trace that they are not going to do anything with other than perhaps log it.

#### Throws

A `throw` is an exception that calling software is expected to handle.

The throw/1 BIF makes a non-local return.
Some other function on the stack is expect to catch the throw and to process the single argument passed to throw.
The argument could simply be an atom but it may be more complex.

Non-local returns may be in deep-nested routines to return an error to the top-level without all the intermediary levels having to check for failure.
Here `{error,Reason}` might be thrown.

Another example might be to throw found (or not found) from a recursive find algorithm on a non-trivial data structure such as a tree.
The top level routine of the search would catch the throw and decided what to return to the caller.
That might be a value passed to the search but the search itself need not know.

It is good practice to catch a throw within the same module.


### Dealing with Exceptions

Throws, errors and even exits can be handled using a `try ... catch ... after ... end` block:

```erlang
    try Expression of
        SuccessPattern1 [Guards] -> Expression1;
           ...
        SuccessPatternN [Guards] -> ExpressionN1
    catch
        TypeOfError:ExceptionPattern1 -> ExceptionExpression1;
           ...
        TypeOfError:ExceptionPatternN -> ExceptionExpressionN
    after
        AnotherExpression
    end
```

The code in Expression is said to be `protected`:  any exception thrown is caught.
Note that it is these exceptions that are caught, not any exception that might be thrown by a success clause.

The TypeOfError is one of `{throw,error,exit}` with throw assumed when no TypeOfError is given.

See the code examples in [Exception](../tut_lyse.erl).

The `after` is code that is always executed regardless of whether an exception was thrown or not.
It does not 'return' a value and so it limited to side-effects.
The classic use of this clause is to release resources such as closing the file that was being read at the time of the exception.

In practice, Expression is a list of comma separated expressions.
The result of the last expression being used for the pattern matching between `of ... catch`.

The result of the `try ... end` block is the result of the success or exception clause that is evaluated.
If the result is irrelevant there may be no success clauses, in which case, the `of` may be omitted.

Note that it is important that no (tail) recursion is used between `try ... of` because
tail recursion assumes no context but context is required is catch is to work.
Tail recursion in success clauses does work but you can always put the tail recursion in a subroutine anyway.


### Wait, there's more!

There is an older `catch` construct that looks simple but has some drawbacks.

```erlang
    1> catch throw(whoa).
    whoa
    2> catch exit(die).
    {'EXIT',die}
    3> catch 1/0.
    {'EXIT',{badarith,[{erlang,'/',[1,0]},
                   {erl_eval,do_apply,5},
                   {erl_eval,expr,5},
                   {shell,exprs,6},
                   {shell,eval_exprs,6},
                   {shell,eval_loop,3}]}}
    4> catch 2+2.
    4
```

Errors and exit have the same representation and throw is as before.

The Reason that accompanies the error as an atom and a stack trace.
The first element of the stack trace is the top-of-stack and shows the module, function and arguments in error.
The other entries are module, function, arity.

You can also retrieve the stack trace using `erlang:get_stacktrace/0`.

One issue with this mechanism is operator precedence:

```erlang
    4> X = catch 4+2.
    * 1: syntax error before: 'catch'
    5> X = (catch 4+2).
    6
```

The other is that if you are unluckly you cannot tell the different between a caught exception and a normal return:

```erlang
    one_or_two(1) -> return;
    one_or_two(2) -> throw(return).
```

illustrates the problem:

```elang
    6> c(exceptions).
    {ok,exceptions}
    7> catch exceptions:one_or_two(1).
    return
    8> catch exceptions:one_or_two(2).
    return
```


### Try a try in a tree

See `has_value/2` in [Tree](../tut_lyse/tree.erl) for an example of using throw to create a non-local return.

Essentially, if the value is found, the atom `true` is thrown and caught.
Otherwise the atom `false` is returned to indicate not found.
There is no checking of return values to decide what to do next as there is in `has_value1/2'.

</body>
</html>
