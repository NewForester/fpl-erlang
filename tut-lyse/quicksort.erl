%
% The code examples from the Recursion section of the Learn You Some Erlang tutorial.
% Copyright (C) 2017 NewForester
% Available under MIT Licence terms
%

-module(quicksort).

-export([quicksort/1, lc_quicksort/1]).

-export([author/0]).

-author("NewForester").

%% simple quicksort algorithm that needs a partition function

quicksort([]) -> [];
quicksort([Pivot|Rest]) ->
    {Smaller, Larger} = partition(Pivot,Rest,[],[]),
    quicksort(Smaller)++[Pivot]++quicksort(Larger).

%% the partition function for quicksort

partition(_,[],Smaller,Larger) -> {Smaller, Larger};
partition(Pivot,[H|T],Smaller,Larger) ->
    if H =< Pivot -> partition(Pivot,T,[H|Smaller],Larger);
       H  > Pivot -> partition(Pivot,T,Smaller,[H|Larger])
    end.

%% simple quicksort using list comprehensions

lc_quicksort([]) -> [];
lc_quicksort([Pivot|Rest]) ->
    lc_quicksort([Smaller || Smaller <- Rest, Smaller =< Pivot])
    ++ [Pivot] ++
    lc_quicksort([Larger || Larger <- Rest, Larger > Pivot]).

%%  --------

author() ->
    author(module_info(attributes)).
author([{author,A}|_]) ->
    A;
author([_|T]) ->
    author(T).

% EOF
