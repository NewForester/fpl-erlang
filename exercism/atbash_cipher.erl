-module(atbash_cipher).

-export([encode/1, decode/1, test_version/0]).

-define(SPACE, $\s).
-define(CHUNK_SIZE, 5).

% ----------------------------------------------

encode(String) ->
    Encode =
        fun
            (H, {C, R})
                when H >= $a, H =< $z ->
                    {C + 1, [$a + $z - H | chunk(C, R)]};
            (H, {C, R})
                when H >= $A, H =< $Z ->
                    {C + 1, [$a + $Z - H | chunk(C, R)]};
            (H, {C, R})
                when H >= $0, H =< $9 ->
                    {C + 1, [H | chunk(C, R)]};
            (_, {C, R}) ->
                    {C, R}
        end,

    {_, R} = lists:foldl(Encode, {0, []}, String),

    lists:reverse(R).

% --

chunk(0, R) ->
        R;
chunk(C, R)
    when C rem ?CHUNK_SIZE == 0 ->
        [?SPACE | R];
chunk(_, R) ->
        R.

% ----------------------------------------------

decode(String) ->
    [atbash(L) || L <- String, L /= ?SPACE].

% --

atbash(Letter)
    when Letter >= $a, Letter =< $z ->
        $a + $z - Letter;
atbash(Letter) ->
        Letter.

% ----------------------------------------------

test_version() -> 1.

%%
%% Incorporating inspiration from three different solutions:
%%
%% - don't be afraid to lay the code out so it can be read
%% - several expressions optimised
%% - use foldl instead of recursion
%%
%% I shied away from too many blank lines.
%% I used macros and got the expression for <space> correct.
%% I rationalised the conversion expression.
%%
%% It seems that although foldl takes a routine as a parameter,
%% you have to use a lambda to wrap up the function call.  D'oh!
%%
%% Using foldl eliminates some overhead but creates other overhead,
%% such as the need to reverse the result at the top level.
%%
