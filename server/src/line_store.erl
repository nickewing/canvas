%% @author Nick Ewing <nick@nickewing.net>
%% @copyright 2009 Nick Ewing.

%% @doc Line Store
%%      See 3.1.2.2 Database in SRS

-module(line_store).
-author('Nicholas E. Ewing <nick@nickewing.net>').

-export([
  connect/0,
  disconnect/1,
  get_lines/3,
  save_line/2,
  get_next_tiles_to_paint/2,
  set_tile_painting_status/3,
  set_tile_last_line/4,
  set_tile_last_painted/3,
  unpainted_line_tiles_count/2,
  archive_line/2,
  
  ensure_tile/3
]).

-include("spatial.hrl").
-include("canvas.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(ls_conn, {db}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% External API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Connect to the database
connect() ->
  {ok, Db} = pgsql:connect(?ls_host, ?ls_db, ?ls_user, ?ls_pass),
  #ls_conn{db = Db}.

disconnect(#ls_conn{db = Db}) ->
  pgsql:terminate(Db).

%% @doc Get lines within Box and after T0
get_lines(#ls_conn{db = Db}, Box, T0) ->
  result_to_lines(pgsql:squery(Db, string:join([
    "SELECT *
    FROM lines
    WHERE ", box_to_db_str(Box) , " && bounding_box
      AND time >= ", util:num_to_str(T0), "
    ORDER BY time ASC"
  ], ""))).

%% @doc Save a line to the database
save_line(#ls_conn{db = Db} = C, #line{points = Points, size = Size, box = Box,
                                       color = Color, time = Time,
                                       user = #user{ip = IP}} = Line) ->
  % save line
  {ok, _Res} = pgsql:squery(Db, string:join([
    "INSERT INTO lines
    (bounding_box, points, size, color, ip, time)
    VALUES (",
            box_to_db_str(Box), ", '",
            points_to_db_str(Points), "', ",
            util:num_to_str(Size), ", ",
            util:num_to_str(Color), ", '",
            IP, "', ",
            util:num_to_str(Time),
          ")"
  ], "")),
  % update tile last_line time
  io:format("~p ~n", [tiles_intersecting_line(Line)]),
  lists:foreach(fun({X, Y}) ->
    ensure_tile(C, X, Y),
    set_tile_last_line(C, X, Y, Time)
  end, tiles_intersecting_line(Line)),
  ok.

%% @doc Gets the next tiles that require painting
get_next_tiles_to_paint(#ls_conn{db = Db}, Limit) ->
  result_to_tiles(pgsql:squery(Db, string:join([
    "SELECT *
    FROM tiles
    WHERE painting_status = 0
      AND last_line > last_painted
    ORDER BY last_line ASC
    LIMIT ", util:num_to_str(Limit)
  ], ""))).

%% TODO: clean these next few functions up.  They're dumb.

%% @doc Set the painting status for a tile
set_tile_painting_status(#ls_conn{db = Db}, TileID, Status) ->
  {ok, _Res} = pgsql:squery(Db, string:join([
    "UPDATE tiles
    SET painting_status = ", util:num_to_str(Status), "
    WHERE id = ", util:num_to_str(TileID)
  ], "")),
  ok.

%% @doc Set the last_line of a tile
set_tile_last_line(#ls_conn{db = Db}, X, Y, Time) ->
  {ok, _Res} = pgsql:squery(Db, string:join([
    "UPDATE tiles
    SET last_line = ", util:num_to_str(Time), "
    WHERE x = ", util:num_to_str(X), "
      AND y = ", util:num_to_str(Y)
  ], "")),
  ok.

%% @doc Set the last_painted of a tile
set_tile_last_painted(#ls_conn{db = Db}, TileID, Time) ->
  {ok, _Res} = pgsql:squery(Db, string:join([
    "UPDATE tiles
    SET last_painted = ", util:num_to_str(Time), "
    WHERE id = ", util:num_to_str(TileID)
  ], "")),
  ok.

%% @doc Find the number of unpainted tiles intersecting a line
unpainted_line_tiles_count(#ls_conn{db = Db}, #line{box = B, time = T}) ->
  {ok, [{"SELECT", _, [[Count]]}]} = pgsql:squery(Db, string:join([
    "SELECT COUNT(*)
     FROM tiles
     WHERE bounding_box && ", box_to_db_str(B), "
       AND last_painted < ", util:num_to_str(T)
  ], "")),
  Count.

%% @doc Archive a line
archive_line(#ls_conn{db = Db}, LineID) ->
  % TODO: make this automic
  {ok, ["INSERT 0 1"]} = pgsql:squery(Db, string:join([
    "INSERT INTO line_archive
    SELECT * FROM lines
    WHERE id = ", util:num_to_str(LineID)
  ], "")),
  {ok, ["DELETE 1"]} = pgsql:squery(Db, string:join([
    "DELETE FROM lines
    WHERE id = ", util:num_to_str(LineID)
  ], "")),
  ok.

%% Make a tile if it doesn't exist
ensure_tile(#ls_conn{db = Db}, X, Y) ->
  {ok, [{"SELECT", _, Res}]} = pgsql:squery(Db, string:join([
    "SELECT 1 FROM tiles
    WHERE x = ", util:num_to_str(X), "
      AND y = ", util:num_to_str(Y)
  ], "")),
  case Res of
    [] ->
      Box = #box{x = X, y = Y, x1 = X + ?tile_size, y1 = Y + ?tile_size},
      {ok, ["INSERT 0 1"]} = pgsql:squery(Db, string:join([
        "INSERT INTO tiles (x, y, bounding_box)
        VALUES (", util:num_to_str(X), ",",
                util:num_to_str(Y), ",",
                box_to_db_str(Box), ")" 
      ], ""));
    _ ->
      ok
  end,
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Interal API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Conversion functions
%%%%%%%%%%%%%%%%%%%%%%%%

%% Convert a #box to a database box-type string
box_to_db_str(#box{x = X, y = Y, x1 = X1, y1 = Y1}) ->
  "box '((" ++ util:num_to_str(X) ++ "," ++ util:num_to_str(Y) ++
    "),(" ++ util:num_to_str(X1) ++ "," ++
    util:num_to_str(Y1) ++ "))'".

% Convert a database box-type string to a #box
db_str_to_box(BinStr) ->
  [X, Y, X1, Y1] = lists:map(fun util:str_to_num/1, 
                             string:tokens(binary_to_list(BinStr), "(),")),
  #box{x = X, y = Y, x1 = X1, y1 = Y1}.

%% Convert a list of points to a string for the database
points_to_db_str(Points) ->
  string:join(lists:map(fun util:num_to_str/1, Points), ",").

%% Convert a point string from the database to a list of points
db_str_to_points(BinStr) ->
  PointStrs = string:tokens(binary_to_list(BinStr), ","),
  lists:map(fun util:str_to_num/1, PointStrs).

%% Convert a result record from the database to a #line
record_to_line([Id, Box, Points, Color, Size, _IP, Time]) ->
  #line{
    id     = Id,
    box    = db_str_to_box(Box),
    points = db_str_to_points(Points),
    color  = Color,
    size   = Size,
    time   = Time
  }.

%% Turn a database result set to a list of lines
result_to_lines({ok, [{_, _, Records}|_]}) ->
  lists:map(fun record_to_line/1, Records).

%% Convert a result record from the database to a #tile
record_to_tile([Id, X, Y, Box, LastPainted, _LastLine, _PStatus]) ->
  #tile{
    id           = Id,
    x            = X,
    y            = Y,
    last_painted = LastPainted,
    box          = db_str_to_box(Box)
  }.

%% Turn a database result set to a list of lines
result_to_tiles({ok, [{_, _, Records}|_]}) ->
  lists:map(fun record_to_tile/1, Records).

%% Bound a coordinate between its tile square
tile_coord(C) ->
  round(util:floor(C / ?tile_size)) * ?tile_size.

%% Find all coordinates of tiles intersecting a given line
tiles_intersecting_line(#line{box = #box{x=BX,y=BY,x1=BX1,y1=BY1}}) ->
  T0X  = tile_coord(BX),  T0Y = tile_coord(BY),
  TFX  = tile_coord(BX1), TFY = tile_coord(BY1),
  XDir = case T0X == TFX of
    true  -> 1.0;
    false -> (TFX - T0X) / abs(TFX - T0X)
  end,
  YDir = case T0Y == TFY of
    true  -> 1.0;
    false -> (TFY - T0Y) / abs(TFY - T0Y)
  end,
  Xsteps = round(util:ceil(abs(TFX - T0X) / ?tile_size)),
  Ysteps = round(util:ceil(abs(TFY - T0Y) / ?tile_size)),
  lists:foldl(fun(I, Tiles) ->
    lists:append(Tiles, lists:map(fun(J) ->
      {round(T0X + XDir * I * ?tile_size),
       round(T0Y + YDir * J * ?tile_size)}
    end, lists:seq(0, Ysteps)))
  end, [], lists:seq(0, Xsteps)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% FIXME: incomplete tests.

%% Tests connect/0 and disconnect/1
connection_test() ->
  Conn = connect(),
  [
    ?assertMatch(#ls_conn{db = _Db}, Conn),
    ?assertEqual(ok, disconnect(Conn))
  ].

%% Tests save_line/2 and get_lines/3
store_test() ->
  Now  = util:now_milliseconds(),
  Conn = connect(),
  L = #line{
    points = [-9000000,-9000001,-9000010,-9000011,-9000012,-9000013],
    size   = 3,
    color  = 16777215,
    time   = Now,
    box    = {box, -9000000,-9000001,-9000012,-9000013},
    user   = #user{ip = "0.0.0.0"}
  },
  save_line(Conn, L),
  ?assertMatch(
    [
      #line{
        points = [-9000000,-9000001,-9000010,-9000011,-9000012,-9000013],
        size   = 3,
        color  = 16777215,
        time   = Now,
        box    = {box, -9000000,-9000001,-9000012,-9000013}
      }
    ],
    get_lines(Conn, {box, -9000000,-9000001,-9000012,-9000013},
              Now - 5000) % 5 milliseconds ago
  ).

box_to_db_str_test() ->
  ?assertEqual("box '((178,169.5),(199,188.5))'",
               box_to_db_str(#box{x = 178, y = 169.5, x1 = 199, y1 = 188.5})).

db_str_to_box_test() ->
  ?assertEqual(#box{x = 178, y = 169.5, x1 = 199, y1 = 188.5},
               db_str_to_box(<<"((178,169.5),(199,188.5))">>)).

points_to_db_str_test() ->
  ?assertEqual("178,169.5,199,188.5",
               points_to_db_str([178,169.5,199,188.5])).

db_str_to_points_test() ->
  ?assertEqual([178,169.5,199,188.5],
               db_str_to_points(<<"178,169.5,199,188.5">>)).

record_to_list_test() ->
  ?assertMatch(
    #line{
      box    = #box{x = 178, y = 169.5, x1 = 199, y1 = 188.5},
      points = [178, 169.5, 199, 188.5],
      color  = 0,
      size   = 3,
      time   = 12242333344
    },
    record_to_line([3, <<"((178,169.5),(199,188.5))">>,
                    <<"178,169.5,199,188.5">>, 0, 3, "0.0.0.0",
                    12242333344])
  ).

tile_coord_test() ->
  [
    ?assertEqual(500,  tile_coord(542.5)),
    ?assertEqual(-500, tile_coord(-30)),
    ?assertEqual(1000, tile_coord(1000.0)),
    ?assertEqual(500,  tile_coord(999.9))
  ].

tiles_intersecting_line_test() ->
  [
    ?assertEqual([{0, 0}], 
      tiles_intersecting_line(#line{box=#box{x=0,y=0,x1=50,y1=50}})),
    ?assertEqual([{0, 0},{500,0}],
      tiles_intersecting_line(#line{box=#box{x=0,y=0,x1=600,y1=50}})),
    ?assertEqual([{0,0},{0,500},{0,1000},{500,0},{500,500},{500,1000},
                  {1000,0},{1000,500},{1000,1000}],
      tiles_intersecting_line(#line{box=#box{x=0,y=0,x1=1000,y1=1000}}))
  ].