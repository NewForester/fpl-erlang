%
% The code examples from the Functional Problem Solving section of the Learn You Some Erlang tutorial.
% Copyright (C) 2017 NewForester
% Available under MIT Licence terms
%

-module(road).

-compile(export_all).

-export([author/0]).

-author("NewForester").

%%

main([FileName]) ->
    {ok, Bin} = file:read_file(FileName),
    Map = parse_map(Bin),
    optimal_path(Map),
    Map = parse_map(Bin),
    Path = optimal_path(Map),
    io:format("~p~n",[Path]),
    erlang:halt().

parse_map(Bin) when is_binary(Bin) ->
    parse_map(binary_to_list(Bin));
parse_map(List) when is_list(List) ->
    Values = [list_to_integer(X) || X <- string:tokens(List,"\r\n\t ")],
    group_vals(Values,[]).

group_vals([],Acc) -> lists:reverse(Acc);
group_vals([A,B,X|Rest],Acc) -> group_vals(Rest,[{A,B,X}|Acc]).

shorter_step({A,B,X},{{DistA,PathA}, {DistB,PathB}}) ->
    OptA1 = {DistA + A,         [{a, A}|PathA]},
    OptA2 = {DistA + B + X,     [{x, X},{b, B}|PathB]},
    OptB1 = {DistB + B,         [{b, B}|PathB]},
    OptB2 = {DistB + A + X,     [{x, X},{a, A}|PathA]},
    {erlang:min(OptA1, OptA2), erlang:min(OptB1, OptB2)}.

optimal_path(Map) ->
    {A,B} = lists:foldl(fun shorter_step/2, {{0,[]},{0,[]}}, Map),
    {_Dist,Path} = if
        hd(element(2,A)) =/= {x,0} -> A;
        hd(element(2,B)) =/= {x,0} -> B
    end,
    lists:reverse(Path).

%%  --------

author() ->
    author(module_info(attributes)).
author([{author,A}|_]) ->
    A;
author([_|T]) ->
    author(T).

% EOF
