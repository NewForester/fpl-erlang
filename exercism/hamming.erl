-module(hamming).

-export([hamming_distance/2, test_version/0]).

hamming_distance(From, To) ->
  hamming_distance(From, To, 0).

hamming_distance([], _, Count) ->
  Count;
hamming_distance(_, [], Count) ->
  Count;
hamming_distance([Hf|Tf], [Ht|Tt], Count) ->
  if Hf /= Ht ->
      Newcount = Count + 1;
    Hf == Ht ->
      Newcount = Count
  end,

  hamming_distance(Tf, Tt, Newcount).

test_version() -> 1.

%%
%% There is a really neat way of doing this but
%% I would not use X and Y as they are too similar:
%% (by reizbar although others had the same idea)
%%

hamming_distance2(From, To) -> hamming2(From, To, 0).

hamming2([X|Xs], [X|Ys], D) -> hamming2(Xs, Ys, D);
hamming2([_|Xs], [_|Ys], D) -> hamming2(Xs, Ys, D+1);
hamming2(_, _, D) -> D.
