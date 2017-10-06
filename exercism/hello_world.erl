-module(hello_world).

-export([hello/0, test_version/0]).

hello() ->
  "Hello, World!".

test_version() -> 2.

%%
%% The exercies asks for a routine that returns a string, not one that prints a string.
%% This 'fits' with a xUnit test harness approach and is also easier in Erlang.
%%