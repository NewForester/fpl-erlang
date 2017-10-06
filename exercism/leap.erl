-module(leap).

-export([leap_year/1, test_version/0]).

leap_year(Year) ->
  case Year of
    _ when Year rem 400 == 0 ->
        true;
    _ when Year rem 100 == 0 ->
        false;
    _ when Year rem 4 == 0 ->
        true;
    _ ->
        false
  end.

test_version() -> 1.

%
% Guards but no patterns, so an if ... might have been better than case ... of.
% Remember full stops, semi-colons and commas.
%
