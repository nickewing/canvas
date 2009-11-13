-module(client_mailbox).
-author('Nick Ewing <nick@nickewing.net>').

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% Server interface
-export([start/0]).

%% internal server state
-record(state, {lines}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Server interface
start()         -> gen_server:start_link(?MODULE, [], []).
cast(M)         -> gen_server:cast(?MODULE, M).
call(M)         -> gen_server:call(?MODULE, M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server Callbacks

%% Called when a connection is made to the server
init([]) ->
  io:format("Client mailbox started.~n"),
  %process_flag(trap_exit, true),
  {ok, #state{lines = []}}.

%% Invoked in response to gen_server:call
handle_call({line, Line}, {Pid, _}, S) ->
  {reply, ok, new_line(Line, S)};

handle_call(get_lines, {Pid, _}, S) ->
  {reply, ok, get_lines(Pid, S)};

handle_call(_Message, _From, S) ->
  {reply, error, S}.

%% Invoked in response to gen_server:cast
handle_cast(_Message, S) -> {noreply, S}.

%% Handle exit of linked processes
% handle_info({'EXIT', Pid, _Reason}, S) ->
%   {noreply, client_logoff(Pid, S)};
handle_info(_Message, S) -> {noreply, S}.

%% Server termination
terminate(_Reason, _S) -> ok.

%% Server code update
code_change(_OldVersion, S, _Extra) -> {ok, S}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal functions

%% add a line to the list
new_line(Line, #state{lines = Lines} = S) ->
  S#state{lines = [Line | Lines]}.

%% send lines to requestor and clear the list
get_lines(Pid, #state{lines = Lines}) ->
  Pid ! {lines, Lines},
  #state{Lines = []}.

