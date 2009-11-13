%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the canvas application.

-module(canvas_app).
-author('Nicholas E. Ewing <nick@nickewing.net>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for canvas.
start(_Type, _StartArgs) ->
  canvas_deps:ensure(),
  canvas_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for canvas.
stop(_State) ->
  ok.
