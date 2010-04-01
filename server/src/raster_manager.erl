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
  tile_drawer/2,
  
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
  {ok, #state{}}.

%% @doc Handle sync. call to manager
%% Unknown message
handle_call(_Message, _From, S) ->
  {reply, error, S}.

%% @doc Handle async. call to manage
handle_cast(test, #state{} = S) ->
  paint_unpainted_tiles(),
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

tile_drawer(#tile{id=TID, x=X, y=Y, last_painted=LastPainted,
                  box=Box}, Parent) ->
  LS  = line_store:connect(),
  % update tile data
  line_store:set_tile_painting_status(LS, TID, 1),
  line_store:set_tile_last_painted(LS, TID, util:now_milliseconds()),
  % fetch all the lines and paint them
  Lines = line_store:get_lines(LS, Box, LastPainted),
  ok = paint_tile_lines(X, Y, lines_to_paint_tuples(Lines, X, Y)),
  % update tile status
  line_store:set_tile_painting_status(LS, TID, 0),
  line_store:disconnect(LS),
  Parent ! {lines, Lines},
  ok.

test() ->
  {ok, R} = start(),
  test(R).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

paint_unpainted_tiles() ->
  LS = line_store:connect(),
  paint_unpainted_tiles(LS),
  line_store:disconnect(LS).
paint_unpainted_tiles(LS) ->
  case line_store:get_next_tiles_to_paint(LS, 10) of
    [] ->
      ok;
    Tiles ->
      io:format("Tiles to paint: ~p~n", [Tiles]),
  
      lists:foreach(fun(T) ->
        spawn(?MODULE, tile_drawer, [T, self()])
      end, Tiles),
  
      LinesToArchive = lists:foldl(fun(_T, List) ->
        receive
          {lines, Lines} ->
            lists:append(List, Lines)
        after 30000 ->
          erlang:throw({drawing_error, Tiles})
        end
      end, [], Tiles),
  
      % archive painted lines if they are painted on all tiles they intersect
      lists:foreach(fun(#line{id = LID} = L) ->
        case line_store:unpainted_line_tiles_count(LS, L) of
          C when C < 1 ->
            io:format("Archive line: ~w ~n", [LID]),
            line_store:archive_line(LS, LID);
          _ -> ok
        end
      end, sets:to_list(sets:from_list(LinesToArchive))),
      
      paint_unpainted_tiles(LS)
  end.

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

tile_path(X) ->
  string:join(["priv/www/var/tiles/", util:num_to_str(X), "/"], "").

tile_filename(Y, Suff) ->
  string:join([util:num_to_str(Y), Suff, ".jpg"], "").

paint_tile_lines(X, Y, Lines) ->
  io:format("~p ~p ~n", [X, Y]),
  Dir      = tile_path(X),
  CurFN    = tile_filename(Y, "_" ++ util:num_to_str(util:now_milliseconds())),
  CurPath  = Dir ++ CurFN,
  LinkFN   = tile_filename(Y, ""),
  LinkPath = Dir ++ LinkFN,
  % determine real location of old file
  OldPath = case file:read_link(LinkPath) of
    {ok, ReadPath} -> ReadPath;
    _ -> none
  end,
  % make sure directories exist
  util:make_dir_path([priv, www, var, tiles, util:num_to_str(X)]),
  % copy old file
  file:copy(LinkPath, CurPath),
  % paint the lines
  Painter = raster_painter:start(),
  raster_painter:start_drawing(Painter, CurPath, ?tile_size, ?tile_size),
  lists:map(fun(L) ->
    raster_painter:draw_line(Painter, L)
  end, Lines),
  ok = raster_painter:stop_drawing(Painter),
  raster_painter:stop(Painter),
  % point symlink to new file
  file:make_symlink(CurFN, LinkPath),
  % sleep a bit to make sure people are done accessing the old file
  timer:sleep(5000),
  % delete old image
  case OldPath of
    none -> ok;
    File -> file:delete(File)
  end,
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

