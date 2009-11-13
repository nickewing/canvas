%%% Client list abtraction
%%%   Currently only support O(n) operations, but should not matter as the lists
%%%   wont tend to have too many entries.  Will optimize if needed.

-module(client_list).
-author('Nick Ewing <nick@nickewing.net>').

-export([
  new/0,
  add/3,
  remove/2,
  filter_by_box/2
]).

-type client_list() :: list().

%% Make a new client_list
-spec new() -> client_list().
new() -> [].

%% Add or update a pid
add(Pid, Box, List) ->
  Existed = lists:keymember(Pid, 1, List),
  {Existed, lists:keystore(Pid, 1, List, {Pid, Box})}.

%% Remove a pid from the list
remove(Pid, List) ->
  lists:keydelete(Pid, 1, List).

%% Return the client pids in a viewing box
filter_by_box(_Box, []) -> [];
filter_by_box(Box, List) ->
  io:format("~w ~w~n", [Box, List]),
  InBox = lists:filter(fun({_Pid, CBox}) ->
    spatial:boxes_intersect(Box, CBox)
  end, List),
  lists:map(fun({Pid, _CBox}) -> Pid end, InBox).