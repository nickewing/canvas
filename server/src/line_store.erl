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
  save_line/2% ,
  %   get_oldest_unpainted_lines/2,
  %   get_unpainted_lines/2
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
  Query = string:join([
            "SELECT *
            FROM lines
            WHERE ", box_to_db_str(Box) , " && bounding_box
              AND time >= ", util:num_to_str(T0), "
            ORDER BY time ASC"
          ], ""),
  result_to_lines(pgsql:squery(Db, Query)).

%% @doc Save a line to the database
save_line(#ls_conn{db = Db}, #line{points = Points, size = Size, box = Box,
                                   color = Color, time = Time,
                                   user = #user{ip = IP}}) ->
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
  {ok, _Res} = pgsql:squery(Db, Query),
  ok.

% set_line_painted(#ls_conn{db = Db}, #line{id = Id}) ->
%   Query = string:join([
%             "UPDATE lines
%             SET painted = true
%             WHERE id = ", util:num_to_str(Id)
%           ], ""),
%   {ok, _Res} = pgsql:squery(Db, Query),
%   ok.
% 
% get_oldest_unpainted_lines(#ls_conn{db = Db}, Limit, Offset) ->
%   Query = string:join([
%             "SELECT *
%             FROM lines
%             WHERE NOT painted
%             ORDER BY time DESC
%             LIMIT ", util:num_to_str(Limit), "
%             OFFSET ", util:num_to_str(Offset)
%           ], ""),
%   result_to_lines(pgsql:squery(Db, Query)).
% 
% get_unpainted_lines(#ls_conn{db = Db}, Box) ->
%   % Query = string:join([
%   %           "SELECT *
%   %           FROM lines
%   %           WHERE ", box_to_db_str(Box) , " && bounding_box
%   %             AND NOT painted
%   %           ORDER BY time ASC"
%   %         ], ""),
%   % result_to_lines(pgsql:squery(Db, Query)),
%   [].

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
record_to_line([Id, Box, Points, Color, Size, _IP, Time, _Painted]) ->
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
                    12242333344, false])
  ).