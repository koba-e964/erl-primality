-module(primality_tests).

-include_lib("eunit/include/eunit.hrl").

is_prime_test_() ->
    [
        fun is_prime_small/0,
        fun is_prime_small2/0,
        fun is_prime_medium/0
    ].


is_prime_small() ->
    Expected = [2, 3, 5, 7, 11, 13, 17, 19],
    Actual = [I || I <- lists:seq(1, 20), primality:is_prime(I)],
    ?assertEqual(Expected, Actual).

is_prime_small2() ->
    Actual = [I || I <- lists:seq(1, 100), primality:is_prime(I)],
    ?assertEqual(25, length(Actual)). % pi(100) = 25

is_prime_medium() ->
    Actual = [I || I <- lists:seq(1, 100000), primality:is_prime(I)],
    ?assertEqual(9592, length(Actual)). % pi(100000) = 9592
