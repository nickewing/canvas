-module(line_store).
-author('Nicholas E. Ewing <nick@nickewing.net>').

-export([connect/0, disconnect/1, get_lines/3, save_line/2, test/0]).

-include("spatial.hrl").
-include("canvas.hrl").

-record(ls_conn, {db}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% External API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Connect to the database
connect() ->
  {ok, Db} = pgsql:connect(?ls_host, ?ls_db, ?ls_user, ?ls_pass),
  #ls_conn{db = Db}.

disconnect(#ls_conn{db = Db}) ->
  pgsql:terminate(Db).

%% Get lines within Box and after T0
get_lines(#ls_conn{db = Db}, Box, T0) ->
  Query = string:join([
            "SELECT *
            FROM lines
            WHERE ", box_to_db_str(Box) , " && bounding_box
              AND time >= ", util:num_to_str(T0), "
            ORDER BY time ASC"
          ], ""),
  {ok, [{_, _, Records}|_]} = pgsql:squery(Db, Query),
  lists:map(fun record_to_line/1, Records).

%% Save a line to the database
save_line(#ls_conn{db = Db}, #line{points = Points, size = Size, box = Box,
                                   color = Color, time = Time,
                                   user = #user{ip = IP}} = L) ->
  Query = string:join([
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
          ], ""),
  io:format("~w~n~p~n~s~n", [Db, L, Query]),
  {ok, Res} = pgsql:squery(Db, Query),
  io:format("~p~n", [Res]),
  Res.

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
  [X, Y, X1, Y1] = string:tokens(binary_to_list(BinStr), "(),"),
  #box{x = X, y = Y, x1 = X1, y1 = Y1}.

%% Convert a list of points to a string for the database
points_to_db_str(Points) ->
  string:join(lists:map(fun util:num_to_str/1, Points), ",").

%% Convert a point string from the database to a list of points
db_str_to_points(BinStr) ->
  PointStrs = string:tokens(binary_to_list(BinStr), ","),
  lists:map(fun util:str_to_num/1, PointStrs).

%% Convert a result record from the database to a #line
record_to_line([_Id, Box, Points, Color, Size, _IP, Time]) ->
  #line{
    box    = db_str_to_box(Box),
    points = db_str_to_points(Points),
    color  = Color,
    size   = Size,
    time   = Time
  }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% FIXME: Tests

test() -> 
  Conn = connect(),
  % L = #line{
  %   points = [0,1,2,3,4,5],
  %   size   = 3,
  %   color  = 16777215,
  %   time   = util:now_microseconds(),
  %   box    = {box, 0, 1, 4, 5},
  %   user   = #user{ip = "0.0.0.0"}
  % },
  % L = {line,[178,169.5,180,172.5,182,176.5,185,179.5,187,180.5,189,181.5,194,186.5,
  %        196,187.5,198,188.5,199,188.5],
  %       3,0,
  %       {box,178,169.5,199,188.5},
  %       1999706107239,
  %       {user,"127.0.0.1"}},
  % save_line(Conn, L).
  Lines = get_lines(Conn, #box{x=-1000, y=-1000, x1=1000, y1=1000}, 0),
  io:format("~p ~n", [Lines]).