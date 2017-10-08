-module(collatz_conjecture).

-export([steps/1, test_version/0]).

steps(N) ->
  collatz_conjecture(N, 0).

collatz_conjecture(N,_) when N =< 0 ->
    {error, "Only strictly positive numbers are allowed"};
collatz_conjecture(1,C) ->
    C;
collatz_conjecture(N,C) when N rem 2 == 1 ->
    collatz_conjecture(3 * N + 1, C + 1);
collatz_conjecture(N,C) ->
    collatz_conjecture(N div 2, C + 1).

test_version() ->
  1.

 %%
 %% Patterns and guards and tail recursive (I hope).
 %% All for nothing without the integer division on the last line.
 %%
