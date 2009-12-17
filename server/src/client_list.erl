%%% Client list abtraction
%%%   Currently only support O(n) operations, but should not matter as the lists
%%%   wont tend to have too many entries.  Will optimize if needed.

-module(client_list).
-author('Nick Ewing <nick@nickewing.net>').

-export([
  new/0, save/4, remove_sid/2,
  fetch_sid/2, sid_exists/2, filter_by_box/2
]).

-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% External API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Make a new client_list
new() ->
  orddict:new().

%% @doc Add or update a pid
save(SID, Box, Client, List) ->
  orddict:store(SID, {Box, Client}, List).

%% @doc Remove a pid from the list
remove_sid(SID, List) ->
  orddict:erase(SID, List).

%% @doc Return client associated with SID
fetch_sid(SID, List) ->
  orddict:fetch(SID, List).

%% @doc Return whether SID exists
sid_exists(SID, List) ->
  orddict:is_key(SID, List).

%% @doc Return the client pids in a viewing box
filter_by_box(_Box, []) -> [];
filter_by_box(Box, List) ->
  In = orddict:filter(fun(_SID, {CBox, _Client}) ->
    case CBox of
      none -> false;
      _    -> spatial:boxes_intersect(Box, CBox)
    end
  end, List),
  lists:map(fun({SID, {_CBox, Client}}) ->
    {SID, Client}
  end, orddict:to_list(In)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new_test() ->
  ?assertEqual(orddict:new(), new()).

save_test() ->
  ?assertEqual(
    [{"abc", {box, client}}],
    save("abc", box, client, new())
  ).

remove_sid_test() ->
  ?assertEqual(
    new(),
    remove_sid("abc", save("abc", box, client, new()))
  ).

fetch_sid_test() ->
  ?assertEqual(
    {box, client},
    fetch_sid("abc", save("abc", box, client, new()))
  ).

sid_exists_test() ->
  ?assert(sid_exists("abc", save("abc", box, client, new()))).

filter_by_box_test() ->
  B1 = {box, 0, 0, 100, 100},
  B2 = {box, -20, -20, -1, -2},
  B3 = {box, -5, -9, 10, 10},
  L  = save("abc", B1, client, new()),
  [
    ?assertEqual([], filter_by_box(B2, L)),
    ?assertEqual([{"abc", client}], filter_by_box(B3, L))
  ].