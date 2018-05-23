-module(sieve).

-export([sieve/1, test_version/0]).

sieve(N) when N < 2 ->
    [];
sieve(N) ->
    A = array:new([{size, N + 1}, {fixed, true}, {default, true}]),

    ArrayToPrimes =
        fun
            (I, true, Acc) when I >= 2 ->
                [I | Acc];
            (_, _, Acc) ->
                Acc
        end,

    lists:reverse(array:foldl(ArrayToPrimes, [], sieve(N, 2, true, A))).

sieve(N, C, _, A) when N < C * C ->
    A;
sieve(N, C, true, A) ->
    sieve(N, C + 1, array:get(C + 1, A), sift(N, C, A));
sieve(N, C, false, A) ->
    sieve(N, C + 1, array:get(C + 1, A), A).

sift(N, 2, A) ->
    sift(N, 2 * 2, 2, A);
sift(N, P, A) ->
    sift(N, P * P, 2 * P, A).

sift(N, I, _, A) when N < I ->
    A;
sift(N, I, S, A) ->
    sift(N, I + S, S, array:set(I, false, A)).

test_version() -> 1.

%%
%%  Genuine sieve solution tidied up
%%

