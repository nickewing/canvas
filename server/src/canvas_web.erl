%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for canvas.

-module(canvas_web).
-author('Nicholas E. Ewing <nick@nickewing.net>').

-export([start/1, stop/0, loop/1]).

%% External API

start(Options) ->
  client_manager:start(),
  code:load_file(canvas_controller),
  {_DocRoot, Options1} = get_option(docroot, Options),
  mochiweb_http:start([{name, ?MODULE}, {loop, fun loop/1} | Options1]).

stop() ->
  mochiweb_http:stop(?MODULE).

loop(Req) ->
  "/" ++ Path = Req:get(path),
  case Req:get(method) of
    Method when Method =:= 'GET'; Method =:= 'HEAD' ->
      io:format("~p: ~w~n", [Path, self()]),
      call_path_action(Req, Path);
    'POST' ->
      case Path of
        _ -> Req:not_found()
      end;
    _ -> Req:respond({501, [], []})
  end.

%% Internal API

get_option(Option, Options) ->
  {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

json_response(X) ->
  {"text/plain", [], [mochijson2:encode(X)]}.

call_path_action(Req, Path) ->
  Action = list_to_atom(Path),
  case erlang:function_exported(canvas_controller, Action, 1) of
    true ->
      Result = canvas_controller:Action(Req),
      Req:ok(json_response(Result));
    false ->
      Req:not_found()
  end.