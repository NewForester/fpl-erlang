%
% The code examples from the Functional Problem Solving section of the Learn You Some Erlang tutorial.
% Copyright (C) 2017 NewForester
% Available under MIT Licence terms
%

-module(calc).

-export([rpn/1, rpn_test/0]).

-export([author/0]).

-author("NewForester").

%%  --------

rpn(L) when is_list(L) ->
        try
            [Result] = lists:foldl(fun rpn/2, [], string:tokens(L, " ")),
            Result
        catch
            error:{badmatch,[_|Extra]} -> list_to_tuple([too_many_operands | Extra]);

            throw:{too_few_operands, Operand} -> {too_few_operands, Operand};
            throw:{bad_operator, Operand} -> {bad_operator, Operand}
        end.

rpn("+",[L,R|S]) -> [R+L|S];
rpn("-",[L,R|S]) -> [R-L|S];
rpn("*",[L,R|S]) -> [R*L|S];
rpn("/",[L,R|S]) -> [R/L|S];
rpn(N,Stack) ->
    try
        [convert(N)|Stack]
    catch
        error:badarg when length(Stack) < 2 ->
            throw({too_few_operands, N});
        error:badarg ->
            throw({bad_operator, N})
    end.

convert(N) ->
    case string:to_float(N) of
        {error,no_float} -> list_to_integer(N);
        {F,_} -> F
    end.

%%  --------

rpn_test() ->
      14 = rpn("10 4 +"),
       6 = rpn("10 4 -"),
      40 = rpn("10 4 *"),
     2.5 = rpn("10 4 /"),
    -2.0 = rpn("10 4 3 + 2 * - 2 /"),

    {too_many_operands,10} = rpn("10 6 4 *"),
    {too_few_operands,"+"} = rpn("10 6 4 * + +"),
    {bad_operator,"%"} = rpn("10 6 4 * %"),
    ok.

%%  --------

author() ->
    author(module_info(attributes)).
author([{author,A}|_]) ->
    A;
author([_|T]) ->
    author(T).

% EOF
