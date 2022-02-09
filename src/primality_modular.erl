-module(primality_modular).

%% API exports
-export([
    power_mod/3
]).

%%====================================================================
%% API functions
%%====================================================================
%% @returns pow(A, B, N)
%% @hidden
-spec power_mod(A :: non_neg_integer(), B :: non_neg_integer(), N :: pos_integer()) -> non_neg_integer().
power_mod(A, B, N) ->
    power_mod_internal(A rem N, B, N, 1 rem N).

%%====================================================================
%% Internal functions
%%====================================================================
power_mod_internal(_A, 0, _N, Acc) ->
    Acc;
power_mod_internal(A, 1, N, Acc) ->
    A * Acc rem N;
power_mod_internal(A, B, N, Acc) when B rem 2 =:= 0 ->
    power_mod_internal(A * A rem N, B div 2, N, Acc);
power_mod_internal(A, B, N, Acc) ->
    power_mod_internal(A * A rem N, B div 2, N, Acc * A rem N).
