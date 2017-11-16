-module(triangle).

-export([kind/3, test_version/0]).

kind(A, B, C) ->
    AllPositive = check_positive(A, B, C),
    InequalityHolds = check_inequality(A, B, C),
    if  not AllPositive ->
            { error, "all side lengths must be positive" };
        not InequalityHolds ->
            { error, "side lengths violate triangle inequality" };
        A == B andalso B == C ->
            equilateral;
        A == B orelse B == C orelse C == A ->
            isosceles;
        true ->
            scalene
    end.

check_inequality(A, B, C) ->
    if A + B =< C -> false;
       B + C =< A -> false;
       C + A =< B -> false;
       true       -> true
    end.

check_positive(A, B, C) ->
    if A =< 0 -> false;
       B =< 0 -> false;
       C =< 0 -> false;
       true   -> true
    end.

test_version() -> 1.

%%
%% My solution is long winded:  some much shorter solutions are just as clear.
%%
%% It is possible to use a guard like (A, A, A) to spot the equilateral.
%%
%% In when clauses, and is represented by , and or is represented by ;.
%%
%% A couple of solutions used lists:sort([A, B, C]), which looked like overkill
%% since they failed to follow through.  That got me thinking to do a second
%% version but @Acaccia had already written it:

kind(X, Y, Z) ->
    case lists:sort([X, Y, Z]) of
        [A, _, _] when A =< 0 -> {error, "all side lengths must be positive"};
        [A, B, C] when C >= A + B -> {error, "side lengths violate triangle inequality"};
        [A, A, A] -> equilateral;
        [A, A, _] -> isosceles;
        [_, A, A] -> isosceles;
        _ -> scalene
    end.

%% except:
%%
        [A, _, A] -> equilateral;
%%
%% and the answer is perfect
%%
