-module(client_manager).
-author('Nick Ewing <nick@nickewing.net>').

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% Server interface
-export([start/0, logon/1, logout/0, join/0, list/0, send_line/2]).

-include("spatial.hrl").

%% internal server state
-record(state, {clients}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Server interface
start()     -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
cast(M)     -> gen_server:cast(?MODULE, M).
call(M)     -> gen_server:call(?MODULE, M).
logon(Box)  -> call({logon, Box}).
logout()    -> call(logout).
join()      -> call(join).
list()      -> call(list).
send_line(Line, SenderSID) -> call({send_line, Line, SenderSID}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server Callbacks

%% Called when a connection is made to the server
init([]) ->
  io:format("Client manager started.~n"),
  process_flag(trap_exit, true),
  {ok, #state{clients = client_list:new()}}.

%% Invoked in response to gen_server:call
handle_call({logon, Box}, {Pid, _}, S) ->
  io:format("logon~p  ~p~n", [Box, Pid]),
  {reply, ok, client_logon(Pid, Box, S)};

handle_call(logout, {Pid, _}, S) ->
  {reply, ok, client_logoff(Pid, S)};

handle_call(join, {_Pid, _}, S) ->
  {SID, S1} = client_join(S),
  {reply, {sid, SID}, S1};

handle_call(list, _From, #state{clients = C} = S) ->
  {reply, C, S};

handle_call({send_line, Line, SenderSID}, _From, #state{clients = C} = S) ->
  distr_lines(C, Line, SenderSID),
  {reply, ok, S};

handle_call(_Message, _From, S) ->
  io:format("err~n"),
  {reply, error, S}.

%% Invoked in response to gen_server:cast
handle_cast(_Message, S) -> {noreply, S}.

%% Handle exit of linked processes
handle_info({'EXIT', Pid, _Reason}, S) ->
  {noreply, client_logoff(Pid, S)};
handle_info(_Message, S) -> {noreply, S}.

%% Server termination
terminate(_Reason, _S) -> ok.

%% Server code update
code_change(_OldVersion, S, _Extra) -> {ok, S}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal functions

client_logon(Pid, Box, #state{clients = C} = S) ->
  {NewlyAdded, C1} = client_list:add(Pid, Box, C),
  case NewlyAdded of
    true  -> link(Pid);
    _     -> ok
  end,
  S#state{clients = C1}.

client_logoff(Pid, #state{clients = C} = S) ->
  S#state{clients = client_list:remove(Pid, C)}.

%% Generate a unique SID, create a new mailbox and add it to the client list
client_join(#state{clients = C} = S) ->
  SID      = generate_sid(C),
  {ok, MB} = client_mailbox:start_new(SID),
  C1       = client_list:add(SID, none, MB, C),
  {SID, S#state{clients = C1}}.

%% Generate a SID and make sure it doesn't already exist (although unlikely)
generate_sid(Clients) ->
  SID = util:term_to_hex(term_to_binary(erlang:now())),
  case client_list:sid_exists(SID, Clients) of
    true  -> generate_sid(Clients);
    false -> SID
  end.

%% Distribute lines to mailboxes and line store
distr_lines(Clients, Line, SenderSID) ->
  Box = spatial:line_box(Line),
  To  = lists:delete(SenderSID, client_list:filter_by_box(Box, Clients)),
  % TODO: send line to linestore
  send_line_to_mailbox(To, Line).

% Send a line to a list of mailboxes
send_line_to_mailbox([], _Line) ->
  ok;
send_line_to_mailbox([H|T], Line) ->
  H ! Line,
  send_line_to_mailbox(T, Line).