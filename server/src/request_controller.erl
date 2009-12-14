-module(request_controller).
-author('Nicholas E. Ewing <nick@nickewing.net>').

-export([
  route_request/2, join/2, update/2,
  send_line/2, send_line_worker/3
]).

-include("canvas.hrl").
-include("spatial.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% External API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

route_request(Req, S) ->
  "/" ++ Path = Req:get(path),
  io:format("~w Routing ~w ~s~n", [self(), Req:get(method), Req:get(path)]),
  case list_to_atom(Path) of
    Action when Action == join;
                Action == update;
                Action == send_line ->
      io:format("~w Action ~w~n", [self(), Action]),
      try ?MODULE:Action(Req, S) of
        Result ->
          Req:ok({"text/plain", [], [Result]})
      catch
        Class:Exception ->
          error_logger:error_report([
            {Class, Exception},
            erlang:get_stacktrace()
          ]),
          Req:ok({"text/plain", [], ["ERROR Invalid request"]})
      end;
    _ ->
      no_route
  end.

%% Distribute lines to mailboxes and line store
send_line_worker(#s_state{cm = CM, ls = LS},
                 #line{box = Box} = Line, SenderSID) ->
  io:format("~w Worker got line: ~p from ~s~n", [self(), Line, SenderSID]),
  To  = lists:keydelete(SenderSID, 1, client_manager:filter_by_box(CM, Box)),
  io:format("~w Worker sending line to: ~p~n", [self(), To]),
  send_line_to_mailboxes(To, Line),
  
  % FIXME:
  Conn = line_store:connect(),
  line_store:save_line(Conn, Line),
  line_store:disconnect(Conn),
  
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Actions
%%%%%%%%%%

join(_Req, #s_state{cm = CM}) ->
  {sid, SID} = client_manager:new_sid(CM),
  resp_ok(SID).

update(Req, S) ->
  {SID, Params} = parse_params_sid(Req),
  Tiles     = parse_tiles(proplists:get_value("t", Params)),
  case fetch_updates(Tiles, SID, S) of
    Lines when is_list(Lines) ->
      resp_ok(
        util:num_to_str(util:now_microseconds()) ++ " " ++
        lines_to_resp_str(Lines)
      );
    cancel ->
      resp_cancelled();
    timeout ->
      resp_timeout()
  end.

send_line(Req, S) ->
  {SID, Params} = parse_params_sid(Req),
  Line = add_line_user(Req, parse_line(proplists:get_value("l", Params))),
  spawn(?MODULE, send_line_worker, [S, Line, SID]),
  resp_ok().

%% Helper Functions
%%%%%%%%%%%%%%%%%

%% Fetch updates for a set of tiles
fetch_updates(Tiles, SID, #s_state{cm = CM}) ->
  NewBox = box_tiles(Tiles),
  io:format("~w Update box: ~p~n", [self(), NewBox]),
  io:format("~w Update tiles: ~p~n", [self(), Tiles]),
  {OldBox, Mailbox} = client_manager:fetch_sid(CM, SID),
  client_mailbox:subscribe(Mailbox, self()),
  Lines = case (NewBox == OldBox) of
            true ->
              io:format("~w Same box~n", [self()]),
              wait_for_lines(?line_wait_timeout);
            false ->
              io:format("~w Update sid ~s: ~p ~w~n",
                        [self(), SID, NewBox, Mailbox]),
              client_manager:update_sid(CM, SID, NewBox, Mailbox),
              client_mailbox:empty_lines(Mailbox),
              
              Conn = line_store:connect(),
              Lines0 = fetch_ls_tiles_lines(Conn, Tiles),
              line_store:disconnect(Conn),
              
              case wait_for_lines(0) of
                Lines1 when is_list(Lines1) ->
                  Lines0 ++ Lines1;
                _ ->
                  Lines0
              end
          end,
  client_mailbox:unsubscribe(Mailbox, self()),
  Lines.

fetch_ls_tiles_lines(Conn, Tiles) ->
  Lines = lists:foldl(fun(Tile, List) ->
    [fetch_ls_tile_lines(Conn, Tile)|List]
  end, [], Tiles),
  lists:flatten(Lines).

fetch_ls_tile_lines(Conn, #tile{box = Box, time = Time}) ->
  line_store:get_lines(Conn, Box, Time).

%% Wait for lines to be sent from mailbox
wait_for_lines(Timeout) ->
  receive
    {lines, Lines, FromMailbox} ->
      client_mailbox:empty_lines(FromMailbox),
      Lines;
    unsubscribed ->
      cancel
  after Timeout ->
    timeout
  end.

%% Set user for line from request data
add_line_user(Req, L) ->
  L#line{user = #user{ip = Req:get(peer)}}.

% Send a line to a list of mailboxes
send_line_to_mailboxes([], _Line) ->
  ok;
send_line_to_mailboxes([{_SID, Mailbox}|T], Line) ->
  %io:format("Sending ~p to ~w~n", [Line, Mailbox]),
  client_mailbox:send_line(Mailbox, Line),
  send_line_to_mailboxes(T, Line).

%% Map tiles to their boxes
tiles_boxes(Tiles) ->
  lists:map(fun(#tile{box = B}) -> B end, Tiles).

%% Calculate the bounding box for a set of tiles
box_tiles(Tiles) ->
  spatial:boxes_box(tiles_boxes(Tiles)).

%% Responses
%%%%%%%%%%%%

%% Build response for OK
resp_ok() ->
  io:format("~w Response OK~n", [self()]),
  <<"OK">>.

%% Build response for OK and a string
resp_ok(Str) ->
  io:format("~w Response OK ~s ~n", [self(), Str]),
  list_to_binary("OK " ++ Str).

%% Build response for CANCELLED
resp_cancelled() ->
  io:format("~w Response CANCELLED~n", [self()]),
  <<"CANCELLED">>.

%% Build response for TIMEOUT
resp_timeout() ->
  io:format("~w Response TIMEOUT~n", [self()]),
  <<"TIMEOUT">>.

%% Request Parsing
%%%%%%%%%%%%%%%%%%

parse_params_sid(Req) ->
  Params  = case Req:get(method) of
    Method when Method =:= 'GET';
                Method =:= 'HEAD' ->
      Req:parse_qs();
    'POST' ->
      Req:parse_post()
  end,
  SID = proplists:get_value("sid", Params),
  {SID, Params}.

%% parse tiles from tile request string.  Example tile str:
%% 123,1234,3341,3412/123423334;4244,452,4523,45234/123412333
parse_tiles(Str) ->
  lists:map(fun parse_tile/1, string:tokens(Str, ";")).

%% parse a #tile from a string
parse_tile(Str) ->
  [PointStr, TimeStr] = string:tokens(Str, "/"),
  [X, Y, X1, Y1]      = parse_points(PointStr),
  Time                = util:str_to_num(TimeStr),
  #tile{box = #box{x = X, y = Y, x1 = X1, y1 = Y1}, time = Time}.

%% parse points from string in the form of "132,412,34123,413"
parse_points(Str) ->
  PointStrs = string:tokens(Str, ","),
  Points = lists:map(fun util:str_to_num/1, PointStrs),
  if
    length(Points) rem 2 == 0 ->
      Points;
    true ->
      erlang:error(badarg)
  end.

%% parse a #line from a string in the format of "24,543,3242,545/DF992A/3"
parse_line(Str) ->
  [PointStr, ColorStr, SizeStr] = string:tokens(Str, "/"),
  Points  = parse_points(PointStr),
  #line{
    points = Points,
    color  = util:str_to_num(ColorStr),
    size   = util:str_to_num(SizeStr),
    time   = util:now_microseconds(),
    box    = spatial:points_box(Points)
  }.


%% Response Building

lines_to_resp_str(Lines) ->
  string:join(lists:map(fun line_to_resp_str/1, Lines), ";").

line_to_resp_str(#line{points = P, size = S, color = C}) ->
  string:join([
    points_to_resp_str(P),
    util:num_to_str(C),
    util:num_to_str(S)
  ], "/").

points_to_resp_str(Points)
    when (length(Points) > 0) and (length(Points) rem 2 == 0) ->
  string:join(lists:map(fun util:num_to_str/1, Points), ",").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% FIXME: Incomplete tests


parse_points_test() ->
  [
    ?assertError(badarg, parse_points("abc")),
    ?assertError(badarg, parse_points("234,abc,23")),
    ?assertError(badarg, parse_points("132,412,34123")),
    ?assertEqual([132,412,34123,413], parse_points("132,412,34123,413")),
    ?assertEqual([23.4,234.5,453.4,34.3], parse_points("23.4,234.5,453.4,34.3"))
  ].

parse_line_test() ->
  [
    ?assertEqual(
      #line{points = [24,543,3242,545], size = 3, color = 14653738},
      parse_points("24,543,3242,545/14653738/3")
    )
  ].

line_to_resp_str_test() ->
  [
    ?assertError(function_clause,
      line_to_resp_str(
        #line{points = [24,543,3242], size = 3, color = 14653738}
      )
    ),
    ?assertEqual(
      "24,543,3242,545/14653738/3",
      line_to_resp_str(
        #line{points = [24,543,3242,545], size = 3, color = 14653738}
      )
    )
  ].

points_to_resp_str_test() ->
  [
    ?assertError(function_clause, points_to_resp_str([1,2,3])),
    ?assertEqual("1,2,3,4", points_to_resp_str([1,2,3,4])),
    ?assertEqual("1.2,2.43,3.9,4", points_to_resp_str([1.2,2.43,3.9,4]))
  ].