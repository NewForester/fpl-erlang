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

    <title>Learn You Some Erlang Notes: Functional Problem Solving</title>
</head>

<body>

# Learn You Some Erlang

## Functional Problem Solving

### Reverse Polish Notation Calculator

Everyday arithmetic uses infix notation.
Alternatives are prefix notation (aka Polish notation) and postfix notation (aka Reverse Polish Notation).

Reverse Polish Notation (RPN) was used in early calculators because it takes up little memory.
Numbers are pushed onto a stack and operators consume pair of numbers and push a result.

This exercise is about building an RPN calculator in Erlang.
Most of it is explained and little is left to the reader.

This is a little disappointing as I had hoped it was now my turn to write some Erlang.
The description included a couple of bits from libraries that would have needed heavy hints otherwise.

An Erlang list can operate as a stack with the head being to top-of-stack.

First, how to parse and expression such as "10 4 3 + 2  * -" ?

```erlang
    1> string:tokens("10 4 3 + 2 * -", " ").
    ["10","4","3","+","2","*","-"]
```

I implemented the basic four functions, not the seven in the tutorial, and ignored the suggestion I add sum and prod functions.
I tried to do this without looking at the answers first.

I added exception handling instead.

Erlangs view "let it crash" is fine if you are the programmer and you are going to fix the problem.
Here we have a user facing calculator that will give very cryptic messages if there are too few or too many
numbers or an operand is mistyped.

This exercise took a whole morning but since it was the first time I have tried to write Erlang without
the answers in front of me, perhaps this was not too bad.
Hard lessons:

  * when you move statements around, you have to redo the full-stops, semi-colons and commas;
  * understand the error messages - particularly the run-time error messages;
  * if you are writing a function for `lists:fold/3` then you are not writing a recursive function;
  * catching exceptions:  look at where they have come from.


### Heathrow to London



</body>
</html>
