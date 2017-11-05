-module(isogram).

-export([is_isogram/1, test_version/0]).

is_isogram(String) ->
    Clean = [C || C <- string:to_lower(String), $a =< C, C =< $z],
    length(Clean) =< 26 andalso check_isogram(Clean).

check_isogram("") ->
    true;
check_isogram([H|T]) ->
    not lists:member(H, T) andalso check_isogram(T).

test_version() ->
    1.

%%
%% This is an improvement over my 'example.erl'.
%% It exploits the fact that an alphabetic string
%% of more than 26 characters cannot be an isogram.
%%
