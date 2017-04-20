%
% The code examples from the Modules section of the Learn You Some Erlang tutorial.
% Copyright (C) 2017 NewForester
% Available under MIT Licence terms
%

-module(useless).

-export([add/2]).

-export([hello/0]).

-export([greet_and_add_two/1]).

-export([author/0]).

-author("NewForester").

%%

add(A,B) ->
    A + B.

hello() ->
    io:format("Hello, world!~n").

greet_and_add_two(X) ->
    hello(),
    add(X,2).

%%  --------

author() ->
    author(module_info(attributes)).
author([{author,A}|_]) ->
    A;
author([_|T]) ->
    author(T).

% EOF
