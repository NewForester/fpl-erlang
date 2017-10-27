-module(space_age).

-export([ageOn/2, test_version/0]).

earthOrbit() -> 31557600. %% seconds

ageOn(earth, Seconds) ->
    Seconds / earthOrbit();
ageOn(mercury, Seconds) ->
    ageOn(earth, Seconds) / 0.2408467;
ageOn(venus, Seconds) ->
    ageOn(earth, Seconds) / 0.61519726;
ageOn(mars, Seconds) ->
    ageOn(earth, Seconds) / 1.8808158;
ageOn(jupiter, Seconds) ->
    ageOn(earth, Seconds) / 11.862615;
ageOn(saturn, Seconds) ->
    ageOn(earth, Seconds) / 29.447498;
ageOn(uranus, Seconds) ->
    ageOn(earth, Seconds) / 84.016846;
ageOn(neptune, Seconds) ->
    ageOn(earth, Seconds) / 164.79132;
ageOn(Planet, Seconds) ->
    undefined.

test_version() -> 1.

%%
%% This was straight forward:  others used the same pattern.
%% Only one used a map structure to store constants.
%%
%% Declaring global constants without declaring a function may not be possible.
%% The only solution that came close declared a macro:
%%
%%    -define(BaseSec, 31557600).
%%
