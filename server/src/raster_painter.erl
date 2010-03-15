%% @author Nick Ewing <nick@nickewing.net>
%% @copyright 2009 Nick Ewing.

%% @doc Raster drawing interface

-module(raster_painter).
-export([
  start/0,
  stop/1,
  init/0,
  start_drawing/4,
  stop_drawing/1,
  draw_line/2
]).

-define(comm_timeout, 30000).
-define(raster_painter_bin, "bin/raster_painter").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% External API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Control
%%%%%%%%%%%

start() ->
  spawn(?MODULE, init, []).

stop(Pid) ->
  Pid ! stop.

init() ->
  process_flag(trap_exit, true),
  Port = open_port({spawn, ?raster_painter_bin}, [{packet, 2}, binary]),
  loop(Port).

%%% Program interface
%%%%%%%%%%%%%%%%%%%%%

start_drawing(Pid, Filename, Width, Height) ->
  call_port(Pid, {start_drawing, Filename, Width, Height}).

stop_drawing(Pid) ->
  call_port(Pid, {stop_drawing}).

draw_line(Pid, Line) ->
  call_port(Pid, {draw_line, Line}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

call_port(Pid, Msg) ->
  Pid ! {call, self(), Msg},
  receive
    {Pid, Result} ->
      Result
  after ?comm_timeout ->
    {timeout, call_port}
  end.

loop(Port) ->
  receive
    {call, Caller, Msg} ->
      Port ! {self(), {command, term_to_binary(Msg)}},
      receive
        {Port, {data, Data}} ->
          Caller ! {self(), binary_to_term(Data)}
      after ?comm_timeout ->
        erlang:error({timeout, call})
      end,
      loop(Port);
    stop ->
      Port ! {self(), close},
      receive
        {Port, closed} ->
          exit(normal)
      after ?comm_timeout ->
        {timeout, stop}
      end;
    {'EXIT', Port, _Reason} ->
      exit(port_terminated)
  end.