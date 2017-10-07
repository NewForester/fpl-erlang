-module(accumulate).

-export([accumulate/2, test_version/0]).

accumulate(Fn, Ls) ->
    [Fn(N) || N <- Ls].

test_version() -> 1.

%%
%% Try solving this without a list comprehension.  I finally see the light.
%% Lists good; list comprehensions better.  Baaa ...
%%
