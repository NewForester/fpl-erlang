-module(sum_of_multiples).

-export([sumOfMultiples/2, test_version/0]).

sumOfMultiples(Multiples, N) ->
    lists:sum(sets:to_list(sets:union([setOfMultiple(M, N - 1) || M <- Multiples]))).

setOfMultiple(M, N) ->
    sets:from_list([X * M || X <- lists:seq(1, N div M)]).

test_version() -> 1.

%%
%% There are two groups of solutions: those than loop on Multiples and then N
%% and those that do it the other way around.
%%
%% The first group (including my solution) involve a merge of multiples.  Each
%% solution used a different mechanism.  I used sets:union, most stuck to lists.
%%
%% The second group uses lists:any and so avoids calculating results that are
%% discarded.  There were pretty much identical.  Mine is below.
%%
%% Someone suggested that for large N and short list M, the first approach
%% might be the more efficient but for the converse the second approach
%% might be better.
%%
%% Note that -spec can be used to give routines a prototype.
%%
%% Here is my second group solution:
%%
sumOfMultiples2(Multiples, N) ->
    lists:sum([X || X <- lists:seq(1, N - 1), isMultiple(X, Multiples)]).

isMultiple(N, Multiples) ->
    lists:any(fun(M) -> N rem M == 0 end, Multiples).
%%
