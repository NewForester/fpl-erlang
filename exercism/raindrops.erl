-module(raindrops).

-export([convert/1, test_version/0]).

sounds() -> [{3, "Pling"}, {5, "Plang"}, {7, "Plong"}].

convert(Number) ->
  Drops = [Sound || {Factor, Sound} <- sounds(), Number rem Factor =:= 0],
  case Drops of
    [] -> integer_to_list(Number);
    _ -> lists:concat(Drops)
  end.

test_version() -> 1.

%%
%% A generic solution inspired by others.
%%
