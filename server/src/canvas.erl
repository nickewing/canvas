%% @author Nick Ewing <nick@nickewing.net>
%% @copyright 2009 Nick Ewing.

%% @doc Canvas server application runner
%%      See 3.1.2 in the SRS

-module(canvas).
-author('Nicholas E. Ewing <nick@nickewing.net>').

-export([start/0, stop/0, stop/1]).

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

stop([Node]) ->
  io:format("Stop:~p~n",[Node]),
  case net_adm:ping(Node) of
    pong -> ok;
    pang ->
      io:format("There is no node with this name~n")
  end,
  rpc:cast(Node, init, stop, []),
  init:stop().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Module was autogenerated by mochiweb framework.  Tests are provided in the
%% framework