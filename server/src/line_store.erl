-module(line_store).
-author('Nicholas E. Ewing <nick@nickewing.net>').

-export([
  connect/0,
  manager/2,
  worker/1,
  get_lines/3,
  save_line/2,
  box_to_db_str/1,
  db_str_to_box_regex/0,
  db_str_to_box/2
]).

-include("spatial.hrl").
-include("canvas.hrl").

-record(ls_conn, {db, manager}).

connect() ->
  {ok, Db} = pgsql:connect(
    ?ls_store_host,
    ?ls_store_db,
    ?ls_store_user,
    ?ls_store_pass
  ),
  W0  = spawn(?MODULE, worker,  [Db]),
  W1  = spawn(?MODULE, worker,  [Db]),
  Man = spawn(?MODULE, manager, [[W0, W1], 0]),
  #ls_conn{db = Db, manager = Man}.

manager(Workers, N) ->
  receive
    M -> lists:nth(N, Workers) ! M
  end,
  N1 = (N + 1) rem length(Workers),
  manager(Workers, N1).

worker(Db) ->
  receive
    {save_line, L} -> save_line(Db, L)
  end,
  worker(Db).


get_lines(#ls_conn{db = Db}, Box, T0) -> 
  %{ok, [{_, _, Data}|_]} = pgsql:pquery(
  Data = pgsql:pquery(
    Db,
    "SELECT *
    FROM coords
    WHERE box '$1' && coord_box
      AND time >= '$2';",
    [box_to_db_str(Box), T0]
  ),
  Data.

save_line(Db, #line{coords = C, size = S, user = #user{ip_addr = IP}}) ->
  {ok, _Res} = pgsql:pquery(
    Db,
    "INSERT INTO lines
    (coords, size, color, ip_addr)
    VALUES ($1, $2, $3, $4)",
    [
      coords_to_db_str(C),
      S,
      C,
      IP
    ]
  ).

box_to_db_str(#box{x = X, y = Y, x1 = X1, y1 = Y1}) ->
  "((" ++ util:num_to_str(X) ++ "," ++ util:num_to_str(Y) ++
    "),(" ++ util:num_to_str(X1) ++ "," ++
    util:num_to_str(Y1) ++ "))".



db_str_to_box_regex() ->
  {ok, R} = re:compile(
    "\\(
      ([0-9]+(\\.[0-9]+)?), ([0-9]+(\\.[0-9]+)?)
    \\),\\(
      ([0-9]+(\\.[0-9]+)?) , ([0-9]+(\\.[0-9]+)?)
    \\)",
    [extended]
  ),
  R.

db_str_to_box(Str, CRegex) ->
  {match, Ms} = re:run(Str, CRegex),
  [_, {X00, X01}, _, {Y00, Y01}, _, {X10, X11}, _, {Y10, Y11}] = Ms,
  {ok, X}  = substr_to_num(Str, X00 + 1, X01),
  {ok, Y}  = substr_to_num(Str, Y00 + 1, Y01),
  {ok, X1} = substr_to_num(Str, X10 + 1, X11),
  {ok, Y1} = substr_to_num(Str, Y10 + 1, Y11),
  #box{x = X, y = Y, x1 = X1, y1 = Y1}.

%% @ private
substr_to_num(Str, Start, Len) ->
  util:str_to_num(string:substr(Str, Start, Len)).

coords_to_db_str(Coords) ->
  string:join(lists:map(fun coord_to_str/1, Coords), ",").

coord_to_str({X, Y}) -> util:num_to_str(X) ++ "," ++ util:num_to_str(Y).

