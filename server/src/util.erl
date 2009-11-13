-module(util).
-author('Nick Ewing <nick@nickewing.net>').

-compile(export_all).

% min function with added unset protection to select first value when starting
min_with_unset(unset, Y) -> Y;
min_with_unset(X, unset) -> X;
min_with_unset(X, Y)     -> erlang:min(X, Y).

% max function with added unset protection to select first value when starting
max_with_unset(unset, Y) -> Y;
max_with_unset(X, unset) -> X;
max_with_unset(X, Y)     -> erlang:max(X, Y).

str_to_term(Str) when is_list(Str) ->
  {ok, T, _} = erl_scan:string(Str ++ "."),
  erl_parse:parse_term(T);
str_to_term(_) ->
  {error, "Not a string"}.

str_to_num(Str) when is_list(Str) ->
  case Resp = str_to_term(Str) of
    {ok, T} when is_float(T) or is_integer(T) ->
      Resp; % all is good
    {ok, _T} ->
      {error, "NaN"}; % valid erlang term, but not a number
    {error, _Error} ->
     Resp % all is wrong
  end;
str_to_num(_) ->
  {error, "Not a string"}.

num_to_str(X) -> mochinum:digits(X).