%
% The code examples from the Recursion section of the Learn You Some Erlang tutorial.
% Copyright (C) 2017 NewForester
% Available under MIT Licence terms
%

-module(recursion).

-export([fac/1, tail_fac/1, len/1, tail_len/1]).
-export([replicate/2, tail_replicate/2, reverse/1, tail_reverse/1]).
-export([sublist/2, tail_sublist/2, zip/2, lenient_zip/2]).

-export([author/0]).

-author("NewForester").

%% a simple recursive implementation of factorial(n) with guards

fac(N)  when N == 0 -> 1;
fac(N)  when N > 0  -> N * fac(N-1).

%% length of a list

len([])    -> 0;
len([_|T]) -> 1 + len(T).

%% a tail recursive implementation of factorial(n)

tail_fac(N) -> tail_fac(N,1).

tail_fac(0,Acc) -> Acc;
tail_fac(N,Acc) when N > 0 -> tail_fac(N-1,N*Acc).

%% length of a list by tail recursion

tail_len(List) -> tail_len(List,0).

tail_len([],Acc) -> Acc;
tail_len([_|T],Acc) -> tail_len(T,Acc+1).

%%  --------

%% replicate any term n times

replicate(0,_) ->
    [];
replicate(N,Term) when N > 0 ->
    [Term|replicate(N-1,Term)].

%% replicate any term n times using tail recursion

tail_replicate(N,Term) ->
    tail_replicate(N,Term,[]).

tail_replicate(0,_,List) ->
    List;
tail_replicate(N,Term,List) ->
    tail_replicate(N-1,Term,[Term|List]).

%% reverse items in a list

reverse([]) -> [];
reverse([H|T]) -> reverse(T)++[H].

%% reverse items in a list using tail recursion

tail_reverse(L) -> tail_reverse(L,[]).

tail_reverse([],Acc) -> Acc;
tail_reverse([H|T],Acc) -> tail_reverse(T,[H|Acc]).

%% return the initial N elements of a list

sublist(_,0) -> [];
sublist([],_) -> [];
sublist([H|T],N) when N > 0 -> [H|sublist(T,N-1)].

%% return the initial N elements of a list using tail recursion

tail_sublist(L,N) -> reverse(tail_sublist(L,N,[])).

tail_sublist(_,0,Acc) -> Acc;
tail_sublist([],_,Acc) -> Acc;
tail_sublist([H|T],N,Acc) when N > 0 -> tail_sublist(T,N-1,[H|Acc]).

%% zip together two lists of the same length

zip([],[]) -> [];
zip([XH|XT],[YH|YT]) -> [{XH,YH}|zip(XT,YT)].

%% zip together two lists using the shorter to determine when to stop

lenient_zip([],_) -> [];
lenient_zip(_,[]) -> [];
lenient_zip([XH|XT],[YH|YT]) -> [{XH,YH}|lenient_zip(XT,YT)].

%%  --------

author() ->
    author(module_info(attributes)).
author([{author,A}|_]) ->
    A;
author([_|T]) ->
    author(T).

% EOF
