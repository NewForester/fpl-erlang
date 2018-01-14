-module(sieve).

-export([sieve/1, test_version/0]).

sieve(N) when N < 2 ->
    [];
sieve(N) ->
    sieve(lists:seq(2,N), [], math:sqrt(N)).

sieve([], P, _) ->
    lists:reverse(P);
sieve([N|S], P, L) when N < L ->
    R = lists:filter(fun(X) -> X rem N /= 0 end, S),
    sieve(R, [N|P], L);
sieve([N|S], P, L) ->
    sieve(S, [N|P], L).

test_version() -> 1.

%%
%%  Add square root optimisation
%%
