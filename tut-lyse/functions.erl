%
% The code examples from the Syntax in Functions section of the Learn You Some Erlang tutorial.
% Copyright (C) 2017 NewForester
% Available under MIT Licence terms
%

-module(functions).

-compile(export_all).

-export([author/0]).

-author("NewForester").

%% return first element of a list

head([H|_]) ->
    H.

%% return second element of a list

second([_,S|_]) ->
    S.

%% return true is arguments are the same

same(X,X) ->
    true;
same(_,_) ->
    false.

%% simplistic date/time validator

valid_time({Date = {Y,M,D}, Time = {H,Min,S}}) ->
    io:format("The Date tuple (~p) says today is:  ~p/~p/~p,~n", [Date,Y,M,D]),
    io:format("The Time tuple (~p) indicates: ~p:~p:~p.~n", [Time,H,Min,S]);
valid_time(_) ->
    io:format("Stop feeding me invalid data!~n").

%%  --------

author() ->
    author(module_info(attributes)).
author([{author,A}|_]) ->
    A;
author([_|T]) ->
    author(T).

% EOF
