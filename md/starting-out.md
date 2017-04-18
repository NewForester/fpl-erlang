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

    <title>Learn You Some Erlang Notes: Starting Out</title>
</head>

<body>

# Learn You Some Erlang

## Starting Out

### The Shell

The Erlang shell can be used to test out most of your Erlang and it also lets you edit stuff live.

To start the shell from a Linux tty enter:

```bash
    $ erl
```

### Shell Commands

The shell uses readline to provide emacs-style command line editing as does bash and other shell programs.
However, its behaviour is more emacs-like than bash or Python and I may want to change the configuration.

It has auto-completion of names, type 'li', press tab and the shell till will probably complete this with 'lists'.

However, this is only name completion.
You should complete a valid Erlang statement before pressing return.

Erlang statements needs a full stop (`.`) to terminate them.
Thus

```erlang
    help().
```

will print information on some of the shell commands available.

To exit the shell, use one of:

```erlang
    q().                % short for ...
    init:stop().
```

If the shell is unresponsive, it is time to break into job control with ^G.
Typing 'h' in response to the prompt lists the option available, including interrupting the unresponsive shell.
killing it, starting another shell or quitting Erlang.

Erlang statements may be separated with a comma (`,`).
All are evaluated but, in the shell, only the result of the last one is printed.

The comma and full stop are a hang over from the days when Erlang was implemented in Prolog.

</body>
</html>
