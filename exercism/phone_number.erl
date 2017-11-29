-module(phone_number).

-export([number/1, areacode/1, pretty_print/1, test_version/0]).

number(String) ->
    number(lists:reverse(String), 0, []).

number([$1], 10, N) ->
    N;
number([], 10, N) ->
    N;
number([], _, _) ->
    "0000000000";
number([H|T], C, N) when H >= $0, H =< $9 ->
    number(T, C + 1, [H | N]);
number([_|T], C, N) ->
    number(T, C, N).

areacode(String) ->
    string:left(number(String), 3).

pretty_print(String) ->
    pretty_print(number(String), 0, []).

pretty_print([], 10, PP) ->
    lists:reverse(PP);
pretty_print([H|T], 0, PP) ->
    pretty_print(T, 1, [H | [$( | PP]]);
pretty_print([H|T], 3, PP) ->
    pretty_print(T, 4, [H | [$\s | [$) | PP]]]);
pretty_print([H|T], 6, PP) ->
    pretty_print(T, 7, [H | [$- | PP]]);
pretty_print([H|T], C, PP) ->
    pretty_print(T, C + 1, [H | PP]).

test_version() -> 1.

%%
%% This iteration is pretty erlang-like because of all the recursion but,
%% as expected most other solutions do a better job.
%%
%% One could use regular expressions but this is erlang.
%%
%% See iteration #2 for better ways of doing number/3 and pretty_print/3.
%%
%% As for the simple areacode, I could have used string:split/3 or string:substr/3.
%%
