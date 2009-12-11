-module(client_manager).
-author('Nick Ewing <nick@nickewing.net>').

-behaviour(gen_server).

-export([
  %% gen_server callbacks
  init/1, handle_call/3, handle_cast/2,
  handle_info/2, terminate/2, code_change/3,
  %% Server interface
  start/0, new_sid/1, list/1,
  fetch_sid/2, update_sid/4, remove_sid/2,
  filter_by_box/2
]).

-include("canvas.hrl").

%% internal server state
-record(state, {clients}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% External API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Server interface
start() ->
  gen_server:start_link(?MODULE, [], []).
cast(CM, M) ->
  gen_server:cast(CM, M).
call(CM, M) ->
  gen_server:call(CM, M).
new_sid(CM) ->
  call(CM, new_sid).
list(CM) ->
  call(CM, list).
fetch_sid(CM, SID) ->
  call(CM, {fetch_sid, SID}).
update_sid(CM, SID, Box, MailBox) ->
  cast(CM, {update_sid, SID, Box, MailBox}).
remove_sid(CM, SID) ->
  cast(CM, {remove_sid, SID}).
filter_by_box(CM, Box) ->
  call(CM, {filter_by_box, Box}).

%%% gen_server Callbacks

%% Called when a connection is made to the server
init([]) ->
  io:format("~w Client manager started.~n", [self()]),
  %process_flag(trap_exit, true),
  {ok, #state{clients = client_list:new()}}.

%% Generate a unique SID, create a new mailbox and add it to the client list
handle_call(new_sid, {_Pid, _}, #state{clients = C} = S) ->
  SID = generate_sid(C),
  {reply, {sid, SID}, create_sid_client(SID, S)};
%% Return the list of current clients
handle_call(list, _From, #state{clients = C} = S) ->
  {reply, C, S};
%% Fetch a client by SID
handle_call({fetch_sid, SID}, _From, S) ->  
  #state{clients = C} = S1 = ensure_sid_exists(SID, S),
  {reply, client_list:fetch_sid(SID, C), S1};
%% Return clients in box
handle_call({filter_by_box, Box}, _From, #state{clients = C} = S) ->
  {reply, client_list:filter_by_box(Box, C), S};
%% Unknown message
handle_call(_Message, _From, S) ->
  {reply, error, S}.

%% Update a client by SID
handle_cast({update_sid, SID, Box, MailBox}, #state{clients = C} = S) ->
  {noreply, S#state{clients = client_list:save(SID, Box, MailBox, C)}};
%% Remove a client by SID
handle_cast({remove_sid, SID}, #state{clients = C} = S) ->
  {noreply, S#state{clients = client_list:remove_sid(SID, C)}};
%% Unknown message
handle_cast(_Message, S) ->
  {noreply, S}.

%% Handle exit of linked processes
handle_info(_Message, S) ->
  {noreply, S}.

%% Server termination
terminate(_Reason, _S) ->
  ok.

%% Server code update
code_change(_OldVersion, S, _Extra) ->
  {ok, S}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ensure_sid_exists(SID, #state{clients = C} = S) ->
  case client_list:sid_exists(SID, C) of
    true  ->
      S;
    false ->
      create_sid_client(SID, S)
  end.

create_sid_client(SID, #state{clients = C} = S) ->
  {ok, MB} = client_mailbox:start_new(self(), SID),
  S#state{clients = client_list:save(SID, none, MB, C)}.

%% Generate a SID and make sure it doesn't already exist (although unlikely)
generate_sid(Clients) ->
  random:seed(now()),
  SID  = util:to_hex(erlang:md5(term_to_binary(random:uniform()))),
  case client_list:sid_exists(SID, Clients) of
    true  ->
      generate_sid(Clients);
    false ->
      SID
  end.