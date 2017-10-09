-module(rna_transcription).

-export([to_rna/1, test_version/0]).

-import(lists, [reverse/1]).

to_rna(Strand) ->
  to_rna(Strand,[]).

to_rna([], RnaStrand) ->
    reverse(RnaStrand);
to_rna(DnaStrand, RnaStrand) ->
    [H|T] = DnaStrand,
    Complement = case H of
        71 -> 67;
        67 -> 71;
        84 -> 65;
        65 -> 85;
        _ -> 6
        end,
    to_rna(T, [Complement|RnaStrand]).

test_version() -> 1.

%%
%% Oops.  No need to use numbers ... $C $G, $A, $T and %U are so much better.
%%
%% Then ...
%%      implement a function to complement a single nucleotide and wrap that
%%      in a list comprehension
%%
%% No need for the recursion or to reverse the result. Sniff.
%%
