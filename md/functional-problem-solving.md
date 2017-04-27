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

This is a simple shortest route exercise.

There are two main roads into London from Heathrow and different segments have different travel times.
There are also a number of side roads that cut through between the main roads.
The use of these to avoid the slower segments can shorten the journey time.
Find the shortest journey.

The problem expresses segment times in triples:  road A, road B and cut through X.
The time from A to A1 is either A1 or B1 + X1 time and so on.
To calculate the shortest route you need to calculate the shortest route from A and
the shortest route from B and take the shorter.

I think I would be tempted to do one, then the other but the exercise calculates them in parallel.
I might also be tempted to calculate them backwards but the exercise calculates the forwards.

First the data has to be read in from a file.
It is not organised into triples.
File i/o is a module and has not been covered by the tutorial.

The read is binary (just a list of characters) so needs splitting on white space into strings;
the strings need converting to integers and then parcelling up into triples.

Solutions to the first two steps were introduced in the RPN Calculator section above.
The third is a quick recursive function.

This is to set things up to use a fold to do the calculation.
This is in shorter_step/2 which deals with tuples of {cumulative_distance, path_so_far}.
The path so far is a list of tuples, representing {segment, segment_length).

The solution all makes sense but I'm not sure I'd dream up the same answer left to my own device
even given the lengthy description.

It is interesting to see erlang:min/2 applied to tuples.
No real explanation of how that works.

At the end, the solution needs to pick the shorter path and there are several ways this could be done.
The one chosen is hd(element(2,AorB)).
This picks the head of the second tuple member so it is looking to see which path has the final leg {x,0}.

I would probably have compared the calculated times.

It finishes by parametrising the module so you could work out the answer to similar problems from the OS command line
provide you had a suitable input file.

So, as with the RPN Calculator, it adds sufficient new material that you do need to know to program Erlang in general
that you would not have been able to solve this problem on your own.

This exercise may not have taken a long as the RPN Calculator but it does not seem that way.

</body>
</html>
