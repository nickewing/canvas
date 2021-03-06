%% @author Nick Ewing <nick@nickewing.net>
%% @copyright 2009 Nick Ewing.

%% @doc Misc. utilities

-module(util).
-author('Nick Ewing <nick@nickewing.net>').

-export([
  reducel/2,
  reducer/2,
  str_to_term/1,
  str_to_num/1,
  num_to_str/1,
  to_hex/1,
  hex_to_int/1,
  now_microseconds/0,
  now_microseconds/1,
  now_milliseconds/0,
  now_milliseconds/1,
  now_seconds/0,
  now_seconds/1,
  make_dir_path/1,
  ceil/1,
  floor/1
]).

-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% External API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% List functions
%%%%%%%%%%%%%%%%%%

%% @doc Reduce list left
reducel(Fn, [H|T]) ->
  lists:foldl(Fn, H, T).

%% @doc Reduce list right
reducer(Fn, L) ->
  lists:foldr(Fn, lists:last(L), lists:sublist(L, length(L) - 1)).

%%% Conversion functions
%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Convert string to term
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

%% @doc Convert string to number
str_to_num(Str) when is_list(Str) and (length(Str) > 0) ->
  case str_to_term(Str) of
    T when is_float(T) or is_integer(T) ->
      T;
    _ ->
      erlang:error(badarg)
  end;
str_to_num(_) ->
  erlang:error(badarg).

%% @doc Convert number to string
num_to_str(X) ->
  mochinum:digits(X).

%% @doc Convert term to hex
to_hex(N) ->
  mochihex:to_hex(N).

%% @doc Convert hex to integer
hex_to_int(N) ->
  mochihex:to_int(N).

%%% Time functions
%%%%%%%%%%%%%%%%%%

%% @doc Return now() in microseconds
now_microseconds() ->
  now_microseconds(now()).

%% @doc Return given now() format time in microseconds
now_microseconds({Macro, Sec, Micro}) ->
  (Macro * 1000000 + Sec) * 1000000 + Micro.

%% @doc Return now() in milliseconds
now_milliseconds() ->
  now_milliseconds(now()).

%% @doc Return given now() format time in milliseconds
now_milliseconds({Macro, Sec, Micro}) ->
  (Macro * 1000000 + Sec) * 1000 + (Micro div 1000).

%% @doc Return now() in seconds
now_seconds() ->
  now_seconds(now()).

%% @doc Return given now() in seconds
now_seconds({Macro, Sec, _Micro}) ->
  Macro * 1000000 + Sec.

%%% File functions
%%%%%%%%%%%%%%%%%%

%% @doc Create full directory path
make_dir_path(PathSegments)
    when is_list(PathSegments) and (length(PathSegments) > 0) ->
  file:make_dir(lists:nth(1, PathSegments)),
  reducel(fun(Dir, Path) ->
    NewPath = filename:absname_join(Path, Dir),
    file:make_dir(NewPath),
    NewPath
  end, PathSegments),
  ok.

%%% Math functions
%%%%%%%%%%%%%%%%%%

floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.

ceil(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% TODO: incomplete tests

reducel_test() ->
  Fn0  = fun(X, Y) -> X / Y end,
  In0  = [3, 5, 74],
  [In0H | In0T] = In0,
  [
    % should act the same as foldl with head and tail of input
    ?assertEqual(reducel(Fn0, In0), lists:foldl(Fn0, In0H, In0T))
  ].

reducer_test() ->
  Fn0  = fun(X, Y) -> X / Y end,
  In0  = [3, 5, 74],
  In0H = lists:last(In0),
  In0T = lists:sublist(In0, length(In0) - 1),
  [
    % should act the same as foldl with last and everything before last of input
    ?assertEqual(reducer(Fn0, In0), lists:foldr(Fn0, In0H, In0T))
  ].

str_to_term_test() ->
  [
    ?assertError(badarg, str_to_term("@#$%^&*")),
    ?assertEqual(2345.4, str_to_term("2345.4")),
    ?assertEqual(2345, str_to_term("2345")),
    ?assertEqual(abc, str_to_term("abc")),
    ?assertEqual('TEST', str_to_term("'TEST'")),
    ?assertEqual({abc, 1.2}, str_to_term("{abc, 1.2}")),
    ?assertEqual([abc, 1.2], str_to_term("[abc, 1.2]")),
    ?assertEqual(<<"abc">>, str_to_term("<<\"abc\">>"))
  ].

str_to_num_test() ->
  [
    ?assertError(badarg, str_to_num(1234)),
    ?assertError(badarg, str_to_num("")),
    ?assertError(badarg, str_to_num("abc")),
    ?assert(str_to_num("2345.4") =:= 2345.4),
    ?assertEqual(
      64573322145243523452345234535234523453425234,
      str_to_num("64573322145243523452345234535234523453425234")
    ),
    ?assertEqual(0, str_to_num("0")),
    ?assertEqual(
      5.123412341234124e+21,
      str_to_num("5123412341234123412341.23")
    )
  ].

num_to_str_test() ->
  [
    ?assertError(badarg, num_to_str("abc")),
    ?assertEqual("0.1", num_to_str(0.1)),
    ?assertEqual("5", num_to_str(5)),
    ?assertEqual(
      "5.123412341234124e+21",
      num_to_str(5123412341234123412341.23)
    ),
    ?assertEqual(
      "5123412341234123412341234234234234234234",
      num_to_str(5123412341234123412341234234234234234234)
    )
  ].

to_hex_test() ->
  [
    ?assertError(badarg, to_hex(10.3)),
    ?assertEqual([], to_hex("")),
    ?assertEqual("616263", to_hex("abc")),
    ?assertEqual("0", to_hex(0)),
    ?assertEqual("a", to_hex(10)),
    ?assertEqual("ab44df0c6fec024c", to_hex(12341234123412341324))
  ].

hex_to_int_test() ->
  [
    ?assertError(badarg, hex_to_int("test")),
    ?assertError(badarg, hex_to_int("")),
    ?assertEqual(10, hex_to_int("A")),
    ?assertEqual(12341234123412341324, hex_to_int("AB44DF0C6FEC024C")),
    ?assertEqual(12341234123412341324, hex_to_int("ab44df0c6fec024c"))
  ].

now_microseconds_test() ->
  Now1 = now_microseconds(),
  Now2 = now_microseconds(now()),
  [
    ?assertError(function_clause, now_microseconds(1234)),
    % Now1 and Now2 are equal (within 5 microseconds)
    ?assert((Now1 - Now2) =< 5)
  ].

now_milliseconds_test() ->
  Now1 = now_milliseconds(),
  Now2 = now_milliseconds(now()),
  [
    ?assertError(function_clause, now_milliseconds(1234)),
    % Now1 and Now2 are equal
    ?assertEqual(Now1, Now2)
  ].

now_seconds_test() ->
  Now1 = now_seconds(),
  Now2 = now_seconds(now()),
  [
    ?assertError(function_clause, now_seconds(1234)),
    % Now1 and Now2 are equal
    ?assertEqual(Now1, Now2)
  ].