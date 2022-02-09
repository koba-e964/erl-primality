-module(primality_miller_rabin).

%% API exports
-export([
    is_prime/1,
    is_prime/2
]).

%%====================================================================
%% API functions
%%====================================================================
%% @doc Performs Miller-Rabin primality check once, with a randomly picked base.
%% @hidden
-spec is_prime(N :: non_neg_integer()) -> boolean().
is_prime(N) when N =< 1 -> false;
is_prime(N) ->
    Rand = rand:uniform(N - 1), % pick a number from [1, N - 1] equiprobably
    is_prime(N, Rand).

%% @doc Performs Miller-Rabin primality check once, with base `Base'. `Base' should be in range [0, `N' - 1].
%% @hidden
-spec is_prime(N :: non_neg_integer(), Base :: non_neg_integer()) -> boolean().
is_prime(N, _Base) when N =< 1 ->
    false;
is_prime(N, _Base) when N rem 2 =:= 0 ->
    N =:= 2;
is_prime(_N, 0) -> true;
is_prime(N, Base) ->
    {C, D} = find_cd(N - 1),
    X = primality_modular:power_mod(Base, D, N),
    case X =:= 1 orelse X =:= N - 1 of
        true -> true;
        false -> mr_loop(N, X, C - 1)
    end.

%%====================================================================
%% Internal functions
%%====================================================================
%% @doc find C, D where N = 2^C * D, where N is an even integer >= 2
%% @hidden
-spec find_cd(N :: pos_integer()) -> {C :: pos_integer(), D :: pos_integer()}.
find_cd(N) -> find_cd(0, N).

find_cd(C, D) when D rem 2 =:= 0 -> find_cd(C + 1, D div 2);
find_cd(C, D) -> {C, D}.

-spec mr_loop(N :: pos_integer(), X :: non_neg_integer(), C :: non_neg_integer()) -> boolean().
mr_loop(N, X, _) when X =:= N - 1 -> true;
mr_loop(_N, _X, 0) -> false;
mr_loop(N, X, C) -> mr_loop(N, X * X rem N, C - 1).
