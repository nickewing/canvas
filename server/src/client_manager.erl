-module(client_manager).
-author('Nick Ewing <nick@nickewing.net>').

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% Server interface
-export([start/0, logon/1, logout/0, list/0, message/2]).

%% internal server state
-record(state, {clients}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Server interface
start()         -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
cast(M)         -> gen_server:cast(?MODULE, M).
call(M)         -> gen_server:call(?MODULE, M).
logon(Box)      -> call({logon, Box}).
logout()        -> call(logout).
message(M, Box) -> call({send_message, M, Box}).
list()          -> call(list).

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

handle_call(list, _From, #state{clients = C} = S) ->
  {reply, C, S};

handle_call({send_message, Msg, Box}, _From, S) ->
  io:format("Server callling dist message: ~p to ~p~n", [Msg, S]),
  respond_to_clients(S, Msg, Box),
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

respond_to_clients(#state{clients = C}, Msg, Box) ->
  io:format("Server dist message: ~p to ~p~n", [Msg, C]),
  respond_to_clients(client_list:filter_by_box(Box, C), Msg).
respond_to_clients([], _Msg) ->
  ok;
respond_to_clients([H|T], Msg) ->
  io:format("Resp ~w~n", [H]),
  H ! {message, Msg},
  respond_to_clients(T, Msg).