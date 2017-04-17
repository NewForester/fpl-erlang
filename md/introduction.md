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

    <title>Learn You Some Erlang Notes: Introduction</title>
</head>

<body>

# Learn You Some Erlang

## Introduction

### About This Tutorial

This tutorial is inspired by [Learn You A Haskell for Great Good](http://learnyouahaskell.com/).
It attempts to make the language attractive and the learning experience friendly.

It is aimed at people with a basic knowledge of programming who may or may not know functional programming.

It attempts to see Erlang for what it is (without hype) by acknowledging its weaknesses as well as lauding its strengths


### So What's Erlang

Erlang is a (real world) functional programming language:
it seeks to respect referential transparency and data immutability as much as is pragmatically possible.

Erlang compiles to byte-code and so, like Java, requires a process virtual machine.

Erlang emphasises concurrency and high reliability through its use of the `actor` model.
Here actors are separate processes with an Erlang virtual machine.
Communication between actors is by means of `messages`:  they share no other state.

Erlang comes with its own development environment comprising:

  * compiler, debugger, profiler and test framework;
  * the Open Telecom Platform (OTP) Framework;
  * a web server and a parser generator;
  * the mnesia database, which is not like other databases;

The Erlang virtual machine allows code to be updated in a running system without interrupting the program.
Code can be distributed with ease and so Erlang scales nicely.
Errors and faults can be managed in a simple but powerful manner.

The Erlang policy is "let it crash" since the Erlang run-time will recover in most cases.


### Don't drink too much Kool-Aid

Erlang processes are light weight and this is good for scalability but this, in itself, means little:
parallelism does not automatically make things faster and the additional overhead works to slow things down.

By the same token, Erlang cannot scale in direct proportion to the number of cores the processor has but
it is better placed to take advantage of multi-core processors than other languages.

Erlang does some things well (robust, scalable web-servers for example) but does other things badly (image processing for example).


### What you need to dive in

You need to install the Erlang environment, that is all.

On many Linux distributions, this can be done by installing a binary package but the tutorial suggests, as expected,
that this may install an out-of-date version and recommends installation from the latest source.


### Where to get help

There are good man pages under Linux.

HTML documentation is available from the [Official Erlang Web Site](http://erlang.org/doc/).

There are good coding practices described [here](http://www.erlang.se/doc/programming_rules.shtml).

The Erlang community site, [trapexit](http://trapexit.org/), has a wiki and mailing lists presented as a forum.

</body>
</html>
