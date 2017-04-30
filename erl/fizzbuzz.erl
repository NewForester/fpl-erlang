%
% Erlang code example:  FizzBuzz
%
% Synopsis:
%       c(fizzbuzz)
%       fizzbuzz:n(N)           % returns fizzbuzz of N
%       fizzbuzz:n((N,M)        % printz fizzbuzz [N,M]
%
% Copyright (C) 2017 NewForester
% Available under MIT Licence terms
%

-module(fizzbuzz).

-export([author/0]).

-export([n/1, n/2]).

-author("NewForester").

%% Returns fizzbuzz for a single number

n(A) when A > 0 andalso is_integer(A) ->
    case A of
        _ when A rem 15 == 0 ->
            fizzbuzz;
        _ when A rem 5 == 0 ->
            buzz;
        _ when A rem 3 == 0 ->
            fizz;
        _ ->
            A
    end.

%% Prints fizzbuzz for a range of numbers

n(A,A) ->
    io:format("{~p,~p}~n", [A, n(A)]);
n(A,B) when A < B ->
    io:format("{~p,~p}~n", [A, n(A)]),
    n(A+1,B).

%%  --------

author() ->
    author(module_info(attributes)).
author([{author,A}|_]) ->
    A;
author([_|T]) ->
    author(T).

% EOF
