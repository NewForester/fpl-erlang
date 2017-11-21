-module(anagram).

-export([find/2, test_version/0]).

find(Word, Candidates) ->
    LW = string:to_lower(Word),
    SW = lists:sort(LW),

    [C || {C, LC} <- [{C, string:to_lower(C)} || C <- Candidates],
        LC /= LW, lists:sort(LC) == SW].

test_version() -> 1.

%%
%% A variety of implementations with some folk implementing their own recursive
%% filter functions, some using lists:filter/2 and only one other using a list
%% comprehension.
%%
%% All but one used lists:sort/1 and that implemented its own equivalent of
%% Python's collections.Counter (the necessary and sufficient subset).
%%
%% All (almost) remembered to use andalso, which I didn't, but most repeated
%% the conversion to lower case and at least one the sort.
%%
%% Two were using a more up to date version of Erlang.  One use string:equal/3
%% and the other string:casefold/1.  The first was a bad choice, the second a
%% good one.
%%
%% One used function heads with a lambda for this filter function.  Interesting
%% but not, in this instance, very effective.
%%
