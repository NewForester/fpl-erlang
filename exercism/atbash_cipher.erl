-module(atbash_cipher).

-export([encode/1, decode/1, test_version/0]).

-define(SPACE, 32).

encode(String) ->
    encode(String, 0, []).

decode(String) ->
    [atbash(L) || L <- String, L /= ?SPACE].

encode([], _, R) ->
    lists:reverse(R);
encode([H|T], C, R) when H >= $a, H =< $z ->
    encode(T, C + 1, atbash(H, C, R));
encode([H|T], C, R) when H >= $A, H =< $Z ->
    encode(T, C + 1, atbash(H, C, R));
encode([H|T], C, R) when H >= $0, H =< $9 ->
    encode(T, C + 1, atbash(H, C, R));
encode([_|T], C, R) ->
    encode(T, C, R).

atbash(Letter) when Letter >= $a, Letter =< $z ->
    $a + 25 - (Letter - $a);
atbash(Letter) when Letter >= $A, Letter =< $Z ->
    $a + 25 - (Letter - $A);
atbash(Letter) ->
    Letter.

atbash(H, C, R) when C rem 5 == 0 andalso C /= 0 ->
    [atbash(H) | [?SPACE | R]];
atbash(H, _, R) ->
    [atbash(H) | R].

test_version() -> 1.

%%
%% erlang has few libraries when compared with other language.  This solution
%% uses just list:reverse and has been refactored until there are no if's or
%% cases, only function headers with guards.
%%
%% There are a couple of trivial things I missed:
%%
%% - the constant representing a space can be written `$ ` but `$\s' is to be preferred
%% - the conversion is, if course, $a + ($z - C) or $a + ($z - C)
%%
%% Here are a couple of things I didn't know but can't use here:
%%
%% - seq($a, $z) is a string representing the alphabet
%% - in a guard [H | T] = String allow you to refer to the whole String
%%
%% My solution uses only lists:reverse().  Other solutions used:
%%
%% - lists:flatten
%% - lists:filtermap
%% - maps:from_list and maps:get
%% - lists:map and lists:filter
%% - lists:split and then string:join
%% - re:replace
%%
%% This first iteration:
%%
%% - is not that easy to read
%% - uses recursion because there are three parameters - use a tuple and foldl
%%
