-module(test_runner).
-author('Nick Ewing <nick@nickewing.net>').

-export([
  test_app_modules/1,
  test_modules/1,
  test_module/1
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% External API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Run all tests available in given apps's modules
test_app_modules([]) ->
  ok;
test_app_modules([H|T] = Apps) when is_list(Apps) ->
  test_app_modules(H),
  test_app_modules(T);
test_app_modules(App) ->
  io:format("~n~nTesting modules in ~w application...~n~n", [App]),
  application:load(App),
  case application:get_key(App, modules) of
    {ok, Mods} ->
      test_modules(Mods);
    _ ->
      io:format("No modules found.~n")
  end,
  io:format("~n").

%% @doc Run tests if they exist on each given nodule
test_modules(List) ->
  lists:foreach(fun try_to_test_module/1, List).

%% @doc Test module
test_module(M) ->
  io:format("Testing ~p module...~n", [M]),
  M:test().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Run test if they exist on given module
try_to_test_module(M) ->
  code:load_file(M),
  case erlang:function_exported(M, test, 0) of
    true ->
      test_module(M);
    _ ->
      ok
  end.