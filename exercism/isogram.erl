-module(isogram).

-export([is_isogram/1, test_version/0]).

foldl(_, Acc, []) -> {false, Acc};
foldl(F, Acc, [H|T]) ->
    case F(H, Acc) of
        {true, A} -> {true, A};
        {false, A} -> foldl(F, A, T)
    end.

is_isogram(C, S) ->
    case string:to_lower(C) of
        L when $a =< L, L =< $z -> test_not_seen(L, S);
        _ -> {false, S}
    end.

test_not_seen(L, S) ->
    case ordsets:is_element(L, S) of
        true -> {true, S};
        false -> {false, ordsets:add_element(L, S)}
    end.

is_isogram(String) ->
    {R, _} = foldl(fun is_isogram/2, ordsets:new(), String),
    not R.

test_version() -> 1.

%%
%%  This solution adds a special foldl that allows early return.
%%
