-module(primality).

%% API exports
-export([
    is_prime/1,
    is_prime/2
]).

%% Type exports
-export_type([
    is_prime_option/0,
    is_prime_options/0
]).

%%====================================================================
%% Internal macros
%%====================================================================
-define(CERTAINTY, certainty).

%%====================================================================
%% Types
%%====================================================================
-type is_prime_option() ::
    {'mode', 'probabilistic' | 'certified'} |
    {'certainty', non_neg_integer()}.

-type is_prime_options() :: [is_prime_option()].

%%====================================================================
%% API functions
%%====================================================================
%% @equiv is_prime(N, [])
%% @doc Checks whether the given integer N is a prime with the default options.
-spec is_prime(N :: non_neg_integer()) -> boolean().
is_prime(N) ->
    is_prime(N, []).

%% @doc Checks whether the given integer N is a prime with the given options.
%%
%% Available options are:
%% <dl>
%%   <dt>`mode'</dt>
%%   <dd>Whether to allow probabilistic errors (`probabilistic') or not (`certified'). The default is `probabilistic'.</dd>
%%   <dt>`certainty'</dt>
%%   <dd>How certain about the result we can be, as in Java's <a href="https://docs.oracle.com/javase/9/docs/api/java/math/BigInteger.html#isProbablePrime-int-">isProbablePrime</a>
%%   If `mode' is `probabilistic', the probability of incorrect return value is at most 2<sup>-`certainty'</sup>.
%%   If `mode' is `certified', this option is ignored.
%%   The default value is `40'.
%%   </dd>
%% </dl>
-spec is_prime(N :: non_neg_integer(), Options :: is_prime_options()) -> boolean().
is_prime(N, _) when N =< 1 ->
    false;

is_prime(N, Options) ->
    % Performs Miller-Rabin algorithm many time
    Certainty = proplists:get_value(?CERTAINTY, Options, 40),
    % Since each run the error rate is at most 1/4, we need to iterate ceil(Certainty/2) times.
    is_prime_internal(N, (Certainty + 1) div 2).

%%====================================================================
%% Internal functions
%%====================================================================
-spec is_prime_internal(N :: non_neg_integer(), NumIter :: non_neg_integer()) -> boolean().
is_prime_internal(_N, 0) ->
    % We're done.
    true;
is_prime_internal(N, NumIter) ->
    primality_miller_rabin:is_prime(N) andalso is_prime_internal(N, NumIter - 1).