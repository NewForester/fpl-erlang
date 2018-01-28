-module(sieve).

-export([sieve/1, test_version/0]).

sieve(N) when N < 2 ->
    [];
sieve(N) ->
    sieve(array:new([{size, N + 1}, {fixed, true}, {default, true}]), 3, N).

sieve(A, C, N) when C * C > N ->
    ArrayToPrimes =
        fun
            (I, _, Acc) when I band 1 == 0 orelse I < 2 ->
                Acc;
            (I, P, Acc) when P ->
                [I | Acc];
            (_, _, Acc) ->
                Acc
        end,

    [2 | array:foldr(ArrayToPrimes, [], A)];
sieve(A, P, N) ->
    sieve(sift(A, P, N), P + 1, N).

sift(A, P, N) ->
    case array:get(P, A) of
        true -> sift(A, P + P + P, P, N);
        false -> A
    end.

sift(A, I, _, N) when I > N ->
    A;
sift(A, I, P, N) ->
    sift(array:set(I, false, A), I + P + P, P, N).

test_version() -> 1.

%%
%%  Genuine sieve that uses an array container
%%
