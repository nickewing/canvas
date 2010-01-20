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
  start/0, test/1
]).

-include("canvas.hrl").
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
  Conn = line_store:connect(),
  
  raster_painter:start_drawing(Painter, "/Users/nick/abc.png", 5000, 5000),
  
  Lines = lines_to_paint_tuples(line_store:get_lines(Conn,
                                                     {box, 0,0,5000,5000}, 0)),
  
  lists:map(fun(L) ->
    raster_painter:draw_line(Painter, L)
  end, Lines),
  
  raster_painter:stop_drawing(Painter),
  
  line_store:disconnect(Conn),
  {noreply, S};
%% Unknown message
handle_cast(_Message, S) ->
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lines_to_paint_tuples(Lines) ->
  lists:map(fun line_to_paint_tuple/1, Lines).

line_to_paint_tuple(#line{points = Points, size = Size, color = Color}) ->
  {Size, Color, Points}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

