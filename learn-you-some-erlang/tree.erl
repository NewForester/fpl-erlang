%
% The code examples from the Recursion section of the Learn You Some Erlang tutorial.
% Copyright (C) 2017 NewForester
% Available under MIT Licence terms
%

-module(tree).

-export([empty/0, insert/3, lookup/2]).
-export([has_value/2]).

-export([author/0]).

-author("NewForester").

%% return a empty node - note use of tagged tuple

empty() -> {node, 'nil'}.

%% insert a new node with Key and Value

insert(Key,Value,{node,'nil'}) ->
    {node, {Key,Value,{node,'nil'},{node,'nil'}}};
insert(NewKey,NewValue,{node,{Key,Value,Smaller,Larger}}) when NewKey < Key ->
    {node, {Key,Value,insert(NewKey,NewValue,Smaller),Larger}};
insert(NewKey,NewValue,{node,{Key,Value,Smaller,Larger}}) when NewKey > Key ->
    {node, {Key,Value,Smaller,insert(NewKey,NewValue,Larger)}};
insert(NewKey,NewValue,{node,{NewKey,_,Smaller,Larger}}) ->
    {node, {NewKey,NewValue,Smaller,Larger}}.

%% lookup a Value given its Key

lookup(_, {node, 'nil'}) ->
    undefined;
lookup(Key, {node, {Key,Value,_,_}}) ->
    {ok, Value};
lookup(Key, {node, {Node,_,Smaller,_}}) when Key < Node ->
    lookup(Key,Smaller);
lookup(Key, {node, {Node,_,_,Larger}}) when Key > Node ->
    lookup(Key,Larger).

%% look for the given value V in the tree

has_value1(_,{node,'nil'}) ->
    false;
has_value1(V,{node,{_,V,_,_}}) ->
    true;
has_value1(V,{node,_,_,Left,Right}) ->
    case has_value1(V,Left) of
        true -> true;
        false -> has_value1(V,Right)
    end.

%% look for the given value V in the tree and use thow to do a non-local return

has_value(V,Tree) ->
    try has_value2(V,Tree) of
        false -> false
    catch
        true -> true
    end.

has_value2(_,{node,'nil'}) ->
    false;
has_value2(V,{node,{_,V,_,_}}) ->
    throw(true);
has_value2(V,{node,_,_,Left,Right}) ->
    has_value2(V,Left),
    has_value2(V,Right).

%%  --------

author() ->
    author(module_info(attributes)).
author([{author,A}|_]) ->
    A;
author([_|T]) ->
    author(T).

% EOF
