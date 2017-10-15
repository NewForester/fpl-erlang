-module(rna_transcription).

-export([to_rna/1, test_version/0]).

-import(lists, [reverse/1]).

to_rna(Strand) ->
    [complement(Amino) || Amino <- Strand].

complement(Amino) ->
    case Amino of
        $G -> $C;
        $C -> $G;
        $T -> $A;
        $A -> $U;
        _ -> Amino
    end.

test_version() -> 1.

%%
%% Second attempt.  Uses:
%%
%%   - character constants
%%   - a list comprehension
%%
