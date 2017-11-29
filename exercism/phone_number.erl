-module(phone_number).

-export([number/1, areacode/1, pretty_print/1, test_version/0]).

number(String) ->
    numb([C || C <- String, C >= $0, C =< $9]).

numb([$1|String]) when length(String) == 10 ->
    String;
numb(String) when length(String) == 10 ->
    String;
numb(String) ->
    string:copies("0", 10).

areacode(String) ->
    string:left(number(String), 3).

pretty_print(String) ->
    [A1, A2, A3, B1, B2, B3, C1, C2, C3, C4] = number(String),

    [$(, A1, A2, A3, $), $\s, B1, B2, B3, $-, C1, C2, C3, C4].

test_version() -> 1.

%%
%% After examination of other solutions.
%%
%% Using a list comprehension for number is something I should have done
%% without looking at other solutions.
%%
%% The solution for pretty_print I would never have thought of.  Pity
%% because that is exactly what Erlang is good at.
%%
