-module(bob).

-export([response_for/1, test_version/0]).

-import(string, [rchr/2, strip/2, to_lower/1, to_upper/1]).

response_for(String) ->
  NoBlanks = strip(String, both),
  Silence = length(NoBlanks) == 0,
  Question = rchr(NoBlanks, $?) == length(NoBlanks),

  Exclamation = (to_upper(NoBlanks) == NoBlanks) and (to_lower(NoBlanks) /= NoBlanks),

  if Silence ->
        "Fine. Be that way!";
    Exclamation ->
        "Whoa, chill out!";
    Question ->
        "Sure.";
    true ->
        "Whatever."
  end.

test_version() -> 1.

%%
%% The online documentation describes Erlang/OTP 20 in which the string library was rewritten.
%% I have Erlang/OTP 15 so have to use the old string library.
%% The confusion this caused led to me importing functions from the string library.
%% I don't think this should be necessary.
%%
%% I could have used string:right/2 or even used the list library instead of string.
%% The result might have looked more like a Functional Program and rather less like Python
%%
