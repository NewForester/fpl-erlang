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

    <title>Learn You Some Erlang Notes: Starting Out (for real)</title>
</head>

<body>

# Learn You Some Erlang

## Starting Out For Real

Erlang is a relatively small and simple language.
It has, at least, that much in common with C.

There are a few basic data types.
These are covered in this section.

The designers of the language deliberately did not provide proper character string handling but
did see fit to provide some interesting bit sequence handling.

In common with other functional programming languages it has atoms,
which have no direct equivalence in other languages.


### Numbers

Erlang handles integer and floating point numbers.

Compound mathematical expression are supported:
the 'normal' operator precedence rules apply.

Examples:

  * `+`       - plus
  * `-`       - minus
  * `*`       - time
  * `/`       - divide
  * `div`     - integer division
  * `rem`     - integer remainder

Plain integers are decimal.
Number bases up to 16 are supported:

  * 2#101010  - binary
  * 8#0777    - octal
  * 16#dead   - hexadecimal


### Named invariants (Invariable Variables)

In Erlang, 'variables' are `invariant`:  once a value has been bound to a name, the value cannot be changed.

You bind a value to a name thus:

```erlang
    One = 1.
```

However, the `=` is actually a comparison operator (and the basis of Erlang pattern matching).
It will raise an exception if the expression to the right and left do not evaluate to the same value.

If the expression on the left is an unbound name, the `=` operator will bind the right hand value to the name.
Thus it is perfectly legal to appear to assign a value to a name more than once provided it is the same value.

Names to which a value may be bound ('variables') begin with an upper case letter.

They may also begin with an underscore (`_`) but, by convention, names beginning with an underscore are place-holders:
they are used where a name must appear but where the value is irrelevant.

Just plain `_` is a special name.
It may appear on the left of an assignment but Erlang will never bind a value to it.
It use becomes apparent below.

It is possible to unbind a name so that a new value may be bound to it
but only in the Erlang shell:  not in production code.
Use:

```erlang
    f(Variable)         % unbind the name Variable
    f()                 % unbind all names
```


### Atoms

In Erlang, an `atom` is a literal or, if you will, a constant with its own name for a value.
About all you can do with atoms is pass them around and compare them.

A name that begins with a lower case letter is an atom (although there are a few that are 'reserved' words).
When used this way they are a bit like anonymous enumerations in C.

Atoms may also be enclosed in single quotes (so atom = 'atom' compares true).
The single quotes must be used when the atom contains spaces, does not begin with a lower case letter
or contains characters other than the ASCII alphanumeric characters plus underscore (`_`) and at (`@`).
When used this way they look a little like string constants but do not expect them to behave that way.

Atoms are used a lot in Erlang but not much on their own.
They take up space but there is no garbage collection of atoms that have outlived their usefulness:
use them modestly.

The following 30 atoms are reserved words in Erlang:

  * Arithmetic operators        - div, rem
  * Boolean operators           - and, andalso, not, or, orelse, xor
  * Bit sequence operators      - band, bnot, bor, bsl, bsr, bxor
  * Function conditionals       - when, if ... end, case ... of ... end
  * Anonymous functions         - fun ... end
  * Exceptions                  - try ... of ... catch ... after ... end
  * Message passing             - receive ... after ... end
  * other                       - begin cond let query


### Boolean Algebra and Comparison Operators

Two (standard) Erlang atoms are `true` and `false`.
They are atoms so they do not have values of 1 and 0 in the conventional sense but
clearly they can be the result of expression such as comparisons.

Erlang has six Boolean operators: `and`, `or`, `xor` (which evaluate both their arguments),
`andalso` and `orelse` (which do not evaluate their right hand argument if there is no need) and `not`.

There are two sets of equality operators:

  * =:= and =/=, which compare type as well as the numeric value
  * == /=, which compare only the numeric value

There are the four relational operators:  <, >, >=, =< (beware this last).

Erlang does not do arbitrary type arithmetic but it will do arbitrary type comparisons.
As I understand it, this allows generic, type-free, algorithms the same way that
implementing operator== and operator< does in C++.

The Erlang ordering is:

```erlang
    number < atom < reference < fun < port < pid < tuple < list < bit string
````

The ordering is not important but that the ordering is well defined is.


### Tuples

In Erlang, a `tuple` is enclosed in `{}` (contrast with Python where they are enclosed in `()`).
A tuple may comprise any number of elements of any type.
Think of them as records of fields with no names.

The simplest tuple comprises two numeric values.  X and Y co-ordinates perhaps.

```erlang
    Point = {10,4}.
```

Unlike other languages, access to values within a tuple is not by index but by pattern matching assignment:

```erlang
    {X,Y} = Point.
```

Assuming X and Y are unbound, this will bind the names X and Y to the two values of Point.
Yes, there must be some clever scope rules that unbind variables automatically.

Erlang also permits:

```erlang
    {X,_} = Point.
```

which, in terms of assignment, ignores the second element of Point and only one name is bound.

In terms of pattern matching, it means match iff the first element of Point matches the value of X:
we do not care what value the second element of point has.

One interesting use of tuples and atoms is to give ascribe units to values:

```erlang
    Temperature = {celsius, 37.4}
```

means 37.4 is a temperature on the Celsius scale.

```erlang
    {fahrenheit, T} = Temperature
    ** exception error: no match of right hand side value {celsius, 37.4}
```

throws an error stopping the program incorrectly interpreting the temperature as if it were on the Fahrenheit scale.

A tuple like this whose first element is an atom is known as a `tagged tuple`.


### Lists

In Erlang, a `list` is enclosed in `[]` (as they are in Python).
They are the most useful data type in Erlang (as they are in other functional programming languages).

The elements of a list may be of any type so they are not really analogous to arrays.

There are two, right associative, operators for lists:

```erlang
    1> [1,2,3] ++ [4,5].
    [1,2,3,4,5]
    2> [1,2,3,4,5] -- [1,2,3].
    [4,5]
```

The length of a list is returned by the eponymous BIF:

```erlang
    3> length([1,2,3] ++ [4,5]).
    5
```

Note:  Erlang has a number of Built In Functions (BIFs) that are not implemented in Erlang
either because they cannot be or, rarely, for performance reasons.
Nowadays, they are implemented in C.

The head of a list is the first element, the rest is the tail:

```erlang
    4> hd([1,2,3,4]).
    1
    5> tl([1,2,3,4]).
    [2,3,4]
```

The cons (`|`) operator (shades of Lisp here) is a very common list operator.

Add a value to a list before the head:

```erlang
    6> List = [2,3,4].
    [2,3,4]
    7> NewList = [1|List].
    [1,2,3,4]
```

and to split a list into head and tail:

```erlang
    5> [Head|Tail] = NewList.
    [1,2,3,4]
    6> Head.
    1
    7> Tail.
    [2,3,4]
```

A list is a recursive data structure comprising a head and a tail that comprises a head and a tail ...

At the end of a proper list is an empty list (`[]`).
Erlang requires proper lists in all circumstances except pattern matching.

In pattern matching, Erlang will accept an improper list such as [Head|Tail]

#### A note on character strings ...

Erlang does not implement a character string type.

A character string is, internally, a list of characters.
So far so good but there are no special string operations.

Not so good is that a list of integers will be printed as a character string unless at least one of them cannot be represented by a character.
This is tricky with Unicode.

However, there are other ways of representing strings.


### List Comprehensions

In Erlang (and other functional programming languages) a `list comprehension` is an alternative to declaring a list by enumeration.
They allow lists to be declared succinctly and so lead to powerful and, in some senses, clear code.

The Erlang syntax is derived from set notation.

The simplest form declares a list built from a `generator expression`:

```erlang
    1> [2*N || N <- [1,2,3,4]].
    [2,4,6,8]
```

Here N is a pattern that takes, in succession, the values of the list [1, 2, 3, 4].

The construction can be constrained to meet certain conditions:

```erlang
    2> [X || X <- [1,2,3,4,5,6,7,8,9,10], X rem 2 =:= 0].
    [2,4,6,8,10]
```

Here the constraint means the result is a list of even integers.

There may be more than one constraint and more than one generator:

```erlang
    3> [X+Y || X <- [1,2], Y <- [2,3]].
    [3,4,4,5]
```

To illustrate the pattern matching aspect of the generator expression:

```erlang
    6> Weather = [{toronto, rain}, {montreal, storms}, {london, fog}, {paris, sun}, {boston, fog}, {vancouver, snow}].
    [{toronto,rain},
    {montreal,storms},
    {london,fog},
    {paris,sun},
    {boston,fog},
    {vancouver,snow}]

    7> FoggyPlaces = [X || {X, fog} <- Weather].
    [london,boston]
```

This is more elegant than:

```erlang
    8> FoggyPlaces = [X || {X, W} <- Weather, W =:= fog].
    [london,boston]
```

Contrast the `<-` operator with `=`.
With `<-` no exception is thrown when 'not fog', as there would be with `=`.
Also note there is some kind of rebinding going on here.


### Bit Syntax

Erlang is unusual in having a clean, simple, syntax for packing and unpacking bits.
This is the telecoms origins showing through.

Bit syntax encloses binary data between `<<`and `>>` (here called a bit sequence) and splits it into readable segments separated by commas.
By default, a segment is a sequence of bits on a byte boundary.

```erlang
    1> Colour = 16#F09A29.
    15768105
    2> Pixel = <<Colour:24>>.
    <<240,154,41>>
```

An RGB colour is 24-bits, conveniently split into 3 byte values.

More interesting is using pattern matching to unpack binary content:

```erlang
    3> Pixels = <<213,45,132,64,76,32,76,0,0,234,32,15>>.
    <<213,45,132,64,76,32,76,0,0,234,32,15>>
```

packs 12 byte values as a 12 x 8-bit sequence and:

```erlang
    4> <<Pix1:24, Pix2:24, Pix3:24, Pix4:24>> = Pixels.
    <<213,45,132,64,76,32,76,0,0,234,32,15>>
```

unpacks it again using pattern matching into 4 x 24-bit values (Pix1, Pix2, Pix3 and Pix4).

It is possible to unpack one of the 24-bit pixel values to get just the first 8 bits:

```erlang
    5> <<R:8, Rest/binary>> = <<Pix1:24>>.
    <<213,45,132>>
    6> R.
    213
```

thanks to the flexibility of the syntax, which allows a binary segment to be given as one of:

  * Value
  * Value:Size
  * Value/TypeSpecifierList
  * Value:Size/TypeSpecifierList

where Size is either bits or bytes depending on the Type in the type specifier list.
In summary, the type specifier list may be built from:

  * Type        - integer | float | binary | bytes | bitstring | bits | utf8 | utf16 | utf32
  * Signedness  - signed | unsigned
  * Endianness  - big | little | native
  * Unit        - written unit:Integer

As an example:

```erlang
    9> <<16#C0A81300:4/integer-unsigned-big-unit:8>>.
    <<192,168,19,0>>
```

Armed with this kind of syntax, parsing a TCP header is easy:

```erlang
    10> <<SourcePort:16, DestinationPort:16,
        AckNumber:32,
        DataOffset:4, _Reserved:4, Flags:8, WindowSize:16,
        CheckSum: 16, UrgentPointer:16,
        Payload/binary>> = SomeBinary.
```

There are 6 BIFs for bit manipulation:  `bsl` (Bit Shift Left), `bsr` (Bit Shift Right), `band`, `bor`, `bxor` and `bnot`.

Erlang also has `bitstrings` (yes, there seems to be some abiguity in the terminology).
Storage-wise, these are much more efficient that lists:

```erlang
    11> <<"this is a bit string!">>.
    <<"this is a bit string!">>
```

Their downside is they are more difficult to use than lists (of characters).
For that reason they tend to be used for string constants and the like.

Aside:  Erlang is not a great language for heavy number crunching.
It is much better suited to soft real-time applications that involve reacting to events and passing messages
(that contain atoms).


### Binary Comprehensions

Erlang `binary comprehensions` are to bit sequences as `list comprehensions` are to lists.

In terms of syntax, `<<` and `>>` replace `[` and `]` and `<=` replaces `<-`:

Using a binary comprehension, line 4 from the previous section could be replaced with:

```erlang
    1> RGB = [ {R,G,B} || <<R:8,G:8,B:8>> <= Pixels ].
    [{213,45,132},{64,76,32},{76,0,0},{234,32,15}]
```

which yields a list of RGB colours and will work with pixel bit sequences of arbitrary length.

The converse is:

```erlang
    2> << <<R:8, G:8, B:8>> || {R,G,B} <- RGB >>.
    <<213,45,132,64,76,32,76,0,0,234,32,15>>
```

which packs a list of RGB colours of arbitrary length into a bit sequence.

At the time the tutorial was written, binary comprehensions were relatlively new to the language and their use was not common.
Therefore the tutorial does nothing more than introduce them.

</body>
</html>
