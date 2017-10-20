-module(strain).

-export([keep/2, discard/2, test_version/0]).

keep(Fn, List) ->
  [X || X <- List, Fn(X)].

discard(Fn, List) ->
  [X || X <- List, not Fn(X)].

test_version() -> 1.

%%
%% Too simple to be true but everyone else's solutions were longer.
%% The standard library provided filter/2 that does this job.
%% It is probably handier than the list comprehension used here
%% when used inside larger functions.
%%
