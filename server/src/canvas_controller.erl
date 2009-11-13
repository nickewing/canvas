-module(canvas_controller).

-export([
  update/1,
  done/1,
  blah/1
]).

-include("config.hrl").

update(Req) ->
  QueryStr = Req:parse_qs(),
  
  case parse_bounding_box(QueryStr) of
    {ok, Box} ->
      client_manager:logon(Box),
      io:format("Update: ~w ~p on box ~p~n", [self(), client_manager:list(), Box]),
      receive
        {message, Msg} -> Msg
      end,
      io:format("Update ~w got: ~p~n", [self(), Msg]),
      client_manager:logout(),
      Msg;
    {error, _Err} ->
      <<"Failed to logon: invalid bounding box.">>
  end.

done(Req) ->
  QueryStr = Req:parse_qs(),
  case parse_bounding_box(QueryStr) of
    {ok, Box} ->
      Msg = parse_message(QueryStr),
      io:format("Message: ~p ~p~n", [Msg, client_manager:list()]),
      client_manager:message(Msg, Box),
      ok;
    {error, _Err} ->
      <<"Failed to send: invalid bounding box.">>
  end.

blah(_Req) -> 
  {ls_conn, Db, _} = line_store:connect(),
  Data = pgsql:squery(
    Db,
    "SELECT *
    FROM coords
    WHERE box '((2345.000,100.12341),(1150.12341,1110.12341))' && coord_box;"
  ),
  io:format("Data: ~p~n", [Data]),
  ok.


%% Parse bounding box from query string
parse_bounding_box(QueryStr) ->
  spatial:comma_str_to_box(proplists:get_value("b", QueryStr)).

parse_message(QueryStr) ->
  list_to_binary(proplists:get_value("message", QueryStr)).