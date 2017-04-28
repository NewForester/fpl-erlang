%
% The code examples from the Higher Order Functions section of the Learn You Some Erlang tutorial.
% Copyright (C) 2017 NewForester
% Available under MIT Licence terms
%

-module(hhfuns).

-compile(export_all).
-compile({no_auto_import,[min/2]}).
-compile({no_auto_import,[max/2]}).

-export([author/0]).

-author("NewForester").

one() -> 1.
two() -> 2.

add(X,Y) -> X() + Y().

%%  --------

% increment all elements in a list

increment([]) -> [];
increment([H|T]) -> [H+1|increment(T)].

% decrement all elements in a list

decrement([]) -> [];
decrement([H|T]) -> [H-1|decrement(T)].

% map abstraction

map(_,[]) -> [];
map(F,[H|T]) -> [F(H)|map(F,T)].

% increment/decrement functions to be given to map

incr(X) -> X+1.
decr(X) -> X-1.

%%  --------

% only keep even numbers

even(L) -> lists:reverse(even(L,[])).

even([],Acc) ->
    Acc;
even([H|T],Acc) when H rem 2 == 0 ->
    even(T,[H|Acc]);
even([_|T],Acc) ->
    even(T,Acc).

% only keep men older than 60

old_men(L) -> lists:reverse(old_men(L,[])).

old_men([],Acc) ->
    Acc;
old_men([Person = {male, Age}|People],Acc) when Age > 60 ->
    old_men(People,[Person|Acc]);
old_men([_|People],Acc) ->
    old_men(People,Acc).

% filter abstraction

filter(Pred,L) -> lists:reverse(filter(Pred,L,[])).

filter(_,[],Acc) ->
    Acc;
filter(Pred,[H|T],Acc) ->
    case Pred(H) of
        true -> filter(Pred,T,[H|Acc]);
        false -> filter(Pred,T,Acc)
    end.

%%  --------

% find maximum of a list

max([H|T]) -> max(T,H).

max([],Max) -> Max;
max([H|T],Max) when H > Max -> max(T,H);
max([_,T],Max) -> max(T,Max).

% find minimum of a list

min([H|T]) -> min(T,H).

min([],Min) -> Min;
min([H|T],Min) when H < Min -> min(T,H);
min([_,T],Min) -> min(T,Min).

% sum of all the elements of a list

sum(L) -> sum(L,0).

sum([],Sum) -> Sum;
sum([H|T],Sum) -> sum(T,H+Sum).

% fold abstrac`tion (non-standard calling sequence ?)

fold(F,[H|T]) -> fold(F,T,H).

fold(_,[],Acc) -> Acc;
fold(F,[H|T],Acc) -> fold(F, T, F(H,Acc)).

%%  --------

author() ->
    author(module_info(attributes)).
author([{author,A}|_]) ->
    A;
author([_|T]) ->
    author(T).

% EOF
