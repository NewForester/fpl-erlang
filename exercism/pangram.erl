-module(pangram).

-export([is_pangram/1, test_version/0]).

foldl(_, Acc, []) -> {false, Acc};
foldl(F, Acc, [H|T]) ->
    case F(H, Acc) of
        {true, A} -> {true, A};
        {false, A} -> foldl(F, A, T)
    end.

is_pangram(C, S) ->
    case string:to_lower(C) of
        L when $a =< L, L =< $z -> test_all_seen(L, S);
        _ -> {false, S}
    end.

test_all_seen(L, S) ->
    S1 = ordsets:add_element(L, S),
    {ordsets:size(S1) == 26, S1}.

is_pangram(Sentence) ->
    {R, _} = foldl(fun is_pangram/2, ordsets:new(), Sentence),
    R.

test_version() -> 1.

%%
%%  This solution adds a special foldl that allows early return.
%%
