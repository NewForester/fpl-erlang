%
% The code examples from the Syntax in Functions section of the Learn You Some Erlang tutorial.
% Copyright (C) 2017 NewForester
% Available under MIT Licence terms
%

-module(what_the_if).

-export([author/0]).

-export([heh_fine/0, oh_god/1, help_me/1]).

-author("NewForester").

%% illustraton of the Erlang if

heh_fine() ->
    if 1 =:= 1 ->
        works
    end,
    if 1 =:= 1; 1 =:= 1 ->
        works
    end,
    if 1 =:= 2, 1 =:= 1 ->
        fails
    end.

%% illustration of the use of true as an 'else' catch all

oh_god(N) ->
    if N =:= 2 ->
        'might succeed';
    true ->
        'always succeeds'
    end.

%% illustration of the using many guards in an if expression

help_me(Animal) ->
    Talk = if
        Animal == cat   -> "meow";
        Animal == beef  -> "mooo";
        Animal == dog   -> "bark";
        Animal == tree  -> "bark";
        true            -> "silence in golden"
    end,
    {Animal, "says " ++ Talk ++ "!"}.

%%  --------

author() ->
    author(module_info(attributes)).
author([{author,A}|_]) ->
    A;
author([_|T]) ->
    author(T).

% EOF
