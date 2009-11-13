%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(canvas).
-author('Nicholas E. Ewing <nick@nickewing.net>').
-export([start/0, stop/0]).

ensure_started(App) ->
  case application:start(App) of
    ok ->
      ok;
    {error, {already_started, App}} ->
      ok
  end.
        
%% @spec start() -> ok
%% @doc Start the canvas server.
start() ->
  canvas_deps:ensure(),
  ensure_started(crypto),
  application:start(canvas).

%% @spec stop() -> ok
%% @doc Stop the canvas server.
stop() ->
  Res = application:stop(canvas),
  application:stop(crypto),
  Res.