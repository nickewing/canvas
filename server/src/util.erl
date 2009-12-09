-module(util).
-author('Nick Ewing <nick@nickewing.net>').

-export([
  reducel/2,
  reducer/2,
  str_to_term/1,
  str_to_num/1,
  num_to_str/1,
  to_hex/1,
  hex_to_int/1
]).

-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% External API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% List functions

reducel(Fn, [H|T]) ->
  lists:foldl(Fn, H, T).

reducer(Fn, L) ->
  lists:foldr(Fn, lists:last(L), lists:sublist(L, length(L) - 1)).

%%% Conversion functions

str_to_term(Str) when is_list(Str) and (length(Str) > 0) ->
  case erl_scan:string(Str ++ ".") of
    {ok, T, _} ->
      case erl_parse:parse_term(T) of
        {ok, T1} ->
          T1;
        _ ->
          erlang:error(badarg)
      end;
    _ ->
      erlang:error(badarg)
  end;
str_to_term(_) ->
  erlang:error(badarg).

str_to_num(Str) when is_list(Str) and (length(Str) > 0) ->
  case str_to_term(Str) of
    T when is_float(T) or is_integer(T) ->
      T;
    _ ->
      erlang:error(badarg)
  end;
str_to_num(_) ->
  erlang:error(badarg).

num_to_str(X) -> mochinum:digits(X).

to_hex(N) ->
  mochihex:to_hex(N).

hex_to_int(N) ->
  mochihex:to_int(N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reducel_test() ->
  Fn0  = fun(X, Y) -> X / Y end,
  In0  = [3, 5, 74],
  [In0H | In0T] = In0,
  [
    % should act the same as foldl with head and tail of input
    ?assert(reducel(Fn0, In0) =:= lists:foldl(Fn0, In0H, In0T))
  ].

reducer_test() ->
  Fn0  = fun(X, Y) -> X / Y end,
  In0  = [3, 5, 74],
  In0H = lists:last(In0),
  In0T = lists:sublist(In0, length(In0) - 1),
  [
    % should act the same as foldl with last and everything before last of input
    ?assert(reducer(Fn0, In0) =:= lists:foldr(Fn0, In0H, In0T))
  ].

str_to_term_test() ->
  [
    ?assertError(badarg, str_to_term("@#$%^&*")),
    ?assert(str_to_term("2345.4") =:= 2345.4),
    ?assert(str_to_term("2345") =:= 2345),
    ?assert(str_to_term("abc") =:= abc),
    ?assert(str_to_term("'TEST'") =:= 'TEST'),
    ?assert(str_to_term("{abc, 1.2}") =:= {abc, 1.2}),
    ?assert(str_to_term("[abc, 1.2]") =:= [abc, 1.2]),
    ?assert(str_to_term("<<\"abc\">>") =:= <<"abc">>)
  ].

str_to_num_test() ->
  [
    ?assertError(badarg, str_to_num(1234)),
    ?assertError(badarg, str_to_num("")),
    ?assertError(badarg, str_to_num("abc")),
    ?assert(str_to_num("2345.4") =:= 2345.4),
    ?assert(
      str_to_num("64573322145243523452345234535234523453425234")
        =:= 64573322145243523452345234535234523453425234
    ),
    ?assert(str_to_num("0") =:= 0),
    ?assert(
      str_to_num("5123412341234123412341.23")
        =:= 5.123412341234124e+21
    )
  ].

num_to_str_test() ->
  [
    ?assertError(badarg, num_to_str("abc")),
    ?assert(num_to_str(5.0) =:= "5.0"),
    ?assert(num_to_str(5) =:= "5"),
    ?assert(
      num_to_str(5123412341234123412341.23) =:= "5.123412341234124e+21"
    ),
    ?assert(
      num_to_str(5123412341234123412341234234234234234234)
        =:= "5123412341234123412341234234234234234234"
    )
  ].

to_hex_test() ->
  [
    ?assertError(badarg, to_hex(10.3)),
    ?assert(to_hex("") =:= []),
    ?assert(to_hex("abc") =:= "616263"),
    ?assert(to_hex(0) =:= "0"),
    ?assert(to_hex(10) =:= "a"),
    ?assert(to_hex(12341234123412341324) =:= "ab44df0c6fec024c")
  ].

hex_to_int_test() ->
  [
    ?assertError(badarg, hex_to_int("test")),
    ?assertError(badarg, hex_to_int("")),
    ?assert(hex_to_int("A") =:= 10),
    ?assert(hex_to_int("AB44DF0C6FEC024C") =:= 12341234123412341324),
    ?assert(hex_to_int("ab44df0c6fec024c") =:= 12341234123412341324)
  ].