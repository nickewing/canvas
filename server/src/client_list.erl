%%% Client list abtraction
%%%   Currently only support O(n) operations, but should not matter as the lists
%%%   wont tend to have too many entries.  Will optimize if needed.

-module(client_list).
-author('Nick Ewing <nick@nickewing.net>').

-export([
  new/0,
  add/4,
  remove/2,
  sid_exists/2,
  filter_by_box/2
]).

%% Make a new client_list
new() -> orddict:new().

%% Add or update a pid
add(SID, Box, Client, List) ->
  orddict:append(SID, {Box, Client}, List).

%% Remove a pid from the list
remove(SID, List) ->
  lists:erase(SID, List).

%% Return whether SID exists
sid_exists(SID, List) ->
  orddict:is_key(SID, List).

%% Return the client pids in a viewing box
filter_by_box(_Box, []) -> [];
filter_by_box(Box, List) ->
  In = orddict:filter(fun(_SID, [{CBox, _Client}]) ->
    case CBox of
      none -> false;
      _    -> spatial:boxes_intersect(Box, CBox)
    end
  end, List),
  lists:map(fun({_SID, {_CBox, Client}}) ->
    Client
  end, orddict:to_list(In)).