%
% The code examples from the Errors and Exceptions section of the Learn You Some Erlang tutorial.
% Copyright (C) 2017 NewForester
% Available under MIT Licence terms
%

-module(exceptions).

-compile(export_all).

-export([author/0]).

-author("NewForester").

throws(F) ->
    try F() of
        _ -> ok
    catch
        Throw -> {throw,caught,Throw}
    end.

errors(F) ->
    try F() of
        _ -> ok
    catch
        error:Error -> {error,caught,Error}
    end.

exits(F) ->
    try F() of
        _ -> ok
    catch
        exit:Exit -> {exit, caught, Exit}
    end.

%%  --------

talk() -> "blah blah".

black_knight(Attack) when is_function(Attack,0) ->
    try Attack() of
        _ -> "None shall pass."
    catch
        throw:slice     -> "It is but a scratch.";
        error:cut_arm   -> "I've had worse.";
        exit:cut_leg    -> "Come on you pansy!";
        _:_             -> "Just a flesh wound."
    end.

sword(1) -> throw(slice);
sword(2) -> erlang:error(cut_arm);
sword(3) -> exit(cut_leg);
sword(4) -> throw(punch);
sword(5) -> exit(cross_bridge).

%%  --------

catcher(X,Y) ->
    case catch X/Y of
        {'EXIT', {badarith,_}} -> "uh oh";
        N-> N
    end.

one_or_two(1) -> return;
one_or_two(2) -> throw(return).

%%  --------

author() ->
    author(module_info(attributes)).
author([{author,A}|_]) ->
    A;
author([_|T]) ->
    author(T).

% EOF
