-module(grains).

-export([square/1, total/0, test_version/0]).

%% simple solution that requires floating point arithmetic and type conversion

pow_square(N) ->
  trunc(math:pow(2, N - 1)).

pow_total() ->
  trunc(math:pow(2, 8 * 8)) - 1.

%% bit shifting - faster because there in no type conversion

bsl_square(N) ->
  1 bsl (N - 1).

bsl_total() ->
  1 bsl 64 - 1.

%% my final offer

square(N) ->
  bsl_square(N).

total() ->
  <<Num:64>> = <<16#FFFFFFFFFFFFFFFF:(64)>>,
  Num.

test_version() -> 1.

%%
%% The instructions had everyone confused.
%% Mine was the only answer that presented first stab and optmised versions.
%% Half used recursion instead of the library function
%% (which on machines without floating point might have been faster).
%% Only one other used bsl.  One used a decimal constant for total/0
%% and two used 65 instead of 64 leading one to wonder if their
%% solutions worked by integer overflow.
%%
