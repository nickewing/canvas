%%% Client list abtraction
%%%   Currently only support O(n) operations, but should not matter as the lists
%%%   wont tend to have too many entries.  Will optimize if needed.

-module(client_list).
-author('Nick Ewing <nick@nickewing.net>').

-export([
  new/0,
  save/4,
  remove_sid/2,
  fetch_sid/2,
  sid_exists/2,
  filter_by_box/2
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% External API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Make a new client_list
new() -> orddict:new().

%% Add or update a pid
save(SID, Box, Client, List) ->
  orddict:store(SID, {Box, Client}, List).

%% Remove a pid from the list
remove_sid(SID, List) ->
  orddict:erase(SID, List).

%% Return client associated with SID
fetch_sid(SID, List) ->
  orddict:fetch(SID, List).

%% Return whether SID exists
sid_exists(SID, List) ->
  orddict:is_key(SID, List).

%% Return the client pids in a viewing box
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