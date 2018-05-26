-module(example).

-export([sieve/1, test_version/0]).

sieve(N) when N < 2 ->
    [];
sieve(N) ->
    sieve(lists:seq(2,N), []).

sieve([], P) ->
    lists:reverse(P);
sieve([N|S], P) ->
    R = lists:filter(fun(X) -> X rem N /= 0 end, S),
    sieve(R, [N|P]).

test_version() -> 1.

%%
%% Example solution submitted to the Exercism Erlang stack.
%%
