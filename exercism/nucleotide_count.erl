-module(nucleotide_count).

-export([count/2, nucleotide_counts/1, test_version/0]).

count(Dna, "G") -> length([X || X <- Dna, X == $G]);
count(Dna, "A") -> length([X || X <- Dna, X == $A]);
count(Dna, "C") -> length([X || X <- Dna, X == $C]);
count(Dna, "T") -> length([X || X <- Dna, X == $T]);
count(Dna, N)   -> erlang:error("Invalid nucleotide", N).

nucleotide_counts(Dna) ->
  [{N, count(Dna, N)} || N <- ["A", "T", "C", "G"]].

test_version() -> 1.

%%
%% This exercise was easy, my solution tidy but there is repetition.
%% What did other people do ?
%%
%% We are down to less than 80 solutions worldwide.  I examined 8.
%% A full half rambled and one even had four lots of repetition.
%% Only two used comprehensions as I did.
%%
%% My nucleotide_counts/1 has a list of (string) lists:
%%
%%    [{N, count(Dna, [N])} || N <- "ATCG"].
%%
%% was the syntax I was groping to not find.
%%
%% To remove my repetition I needed to ask is N one of "ATCG".
%% The prefer solution seemed to be:
%%
%%    lists:member(N, "ATCG")
%%
%% although string::str("ATCG", N) was also used.
%%
%% Also:
%%
%%    -define(NUCLEOTIDES, "ATCG").
%%
%% was a nice touch.
%%
