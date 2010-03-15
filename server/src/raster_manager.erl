%% @author Nick Ewing <nick@nickewing.net>
%% @copyright 2009 Nick Ewing.

%% @doc Manages rasterization of drawing data

-module(raster_manager).
-author('Nick Ewing <nick@nickewing.net>').

-behaviour(gen_server).

-export([
  %% gen_server callbacks
  init/1, handle_call/3, handle_cast/2,
  handle_info/2, terminate/2, code_change/3,
  %% Server interface
  start/0, test/1,
  %% Worker
  worker_start/3,
  
  
  
  test/0
]).

-include("canvas.hrl").
-include("spatial.hrl").
%-include_lib("eunit/include/eunit.hrl").

%% internal server state
-record(state, {painter}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% External API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Server interface
%%%%%%%%%%%%%%%%%%%%

%% @doc Start a new client manager
start() ->
  gen_server:start_link(?MODULE, [], []).
%% @doc Send an async. message to a manager
cast(Pid, M) ->
  gen_server:cast(Pid, M).
%% @doc Send a sync. message to a manager
call(Pid, M) ->
  gen_server:call(Pid, M).
%% @doc Fetch a client by SID
test(Pid) ->
  cast(Pid, test).

%%% gen_server Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Called when a connection is made to the server
init([]) ->
  io:format("~w Raster manager started.~n", [self()]),
  {ok, #state{painter = raster_painter:start()}}.

%% @doc Handle sync. call to manager
%% Unknown message
handle_call(_Message, _From, S) ->
  {reply, error, S}.

%% @doc Handle async. call to manage
handle_cast(test, #state{painter = Painter} = S) ->
  LSConn = line_store:connect(),
  Lines = line_store:get_oldest_unpainted_lines(LSConn, 5),
  
  lists:map(fun(L) ->
    spawn(?MODULE, worker_start, [LSConn, L, self()])
  end, Lines),
  
  % TODO: CLOSE LINESTORE AFTER ALL WORKERS ARE DONE!
  % line_store:disconnect(LSConn)
  
  {noreply, S};
%% Unknown message
handle_cast(Message, S) ->
  io:format("Unknown message ~p~n", [Message]),
  {noreply, S}.

%% @doc Handle other messages sent to manager
handle_info(_Message, S) ->
  {noreply, S}.

%% @doc Server termination
terminate(_Reason, _S) ->
  ok.

%% @doc Server code update
code_change(_OldVersion, S, _Extra) ->
  {ok, S}.

%%% Worker functions
%%%%%%%%%%%%%%%%%%%%

worker_start(LSConn, #line{box = #box{x = X, y = Y} = Box}, Parent) ->
  Lines = line_store:get_unpainted_lines(LSConn, Box),
  paint_tile_lines(X, Y, lines_to_paint_tuples(Lines, X, Y)).




test() ->
  {ok, R} = start(),
  test(R).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lines_to_paint_tuples(Lines, OriginX, OriginY) ->
  lists:map(fun(#line{points = Points, size = Size, color = Color}) ->
    {Size, Color, transpose_paint_points(Points, OriginX, OriginY)}
  end, Lines).

transpose_paint_points(Points, OriginX, OriginY) ->
  {Res, _} = lists:mapfoldl(fun(V, Dim) ->
    case Dim of
      x -> {V - OriginX, y};
      y -> {V - OriginY, x}
    end
  end, x, Points),
  Res.

ensure_tile_dirs(X, _Y) ->
  util:make_dir_path([var, tiles, util:num_to_str(X)]).

tile_path(X, Y) ->
  "var/tiles/" ++ util:num_to_str(X) ++ "/" ++ util:num_to_str(Y) ++ ".png".

paint_tile_lines(BoxX, BoxY, Lines) ->
  Painter = raster_painter:start(),
  
  io:format("~p~n", [tile_path(BoxX, BoxY)]),
  
  ensure_tile_dirs(BoxX, BoxY),
  
  raster_painter:start_drawing(Painter, tile_path(BoxX, BoxY),
                               ?tile_size, ?tile_size),
  
  lists:map(fun(L) ->
    raster_painter:draw_line(Painter, L)
  end, Lines),
  
  raster_painter:stop_drawing(Painter),
  raster_painter:stop(Painter).

point_tile(X, Y) ->
  {(X div ?tile_size) * ?tile_size, (Y div ?tile_size) * ?tile_size}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

