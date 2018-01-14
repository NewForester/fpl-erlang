-module(isogram).

-export([is_isogram/1, test_version/0]).

is_isogram(String) ->
    is_isogram(String, sets:new()).

is_isogram([], _) ->
    true;
is_isogram([H|T], LS) when $a =< H, H =< $z ->
    not sets:is_element(H, LS) andalso is_isogram(T, sets:add_element(H, LS));
is_isogram([H|T], LS) when $A =< H, H =< $Z ->
    L = string:to_lower(H),
    not sets:is_element(L, LS) andalso is_isogram(T, sets:add_element(L, LS));
is_isogram([H|T], LS) ->
    is_isogram(T, LS).

test_version() -> 1.

%%
%% Check current letter against those already processed, not those still to process
%%
