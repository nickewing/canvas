-module(client_mailbox).
-author('Nick Ewing <nick@nickewing.net>').

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% Server interface
-export([start_new/1, empty_lines/1, subscribe/1, send_line/2]).

-include("canvas.hrl").

%% internal server state
-record(state, {sid, lines, last_update, listener}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% External API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Server interface
start_new(SID)  -> gen_server:start_link(?MODULE, [SID], []).
%cast(MB, M)    -> gen_server:cast(MB, M).
call(MB, M)    -> gen_server:call(MB, M).
empty_lines(MB) -> call(MB, empty_lines).
subscribe(MB)   -> call(MB, subscribe).
send_line(MB, Line)  -> call(MB, Line).

%%% gen_server Callbacks

%% Called when a connection is made to the server
init([SID]) ->
  timer:send_interval(?mailbox_timeout_interval, kill_on_timeout),
  io:format("Client mailbox ~w, ~s started.~n", [self(), SID]),
  {ok, #state{sid = SID, lines = [], last_update = now()}}.

%% Invoked in response to gen_server:call

handle_call(#line{} = Line, {_Pid, _}, S) ->
  {reply, ok, handle_new_line(Line, set_last_update(S))};

handle_call(subscribe, {Pid, _}, S) ->
  {reply, ok, handle_subscribe(Pid, set_last_update(S))};

handle_call(empty_lines, {_Pid, _}, S) ->
  {reply, ok, handle_empty_lines(set_last_update(S))};

handle_call(_Message, _From, S) ->
  {reply, error, S}.

%% Invoked in response to gen_server:cast
handle_cast(_Message, S) -> {noreply, S}.

%% Handle other messages
handle_info(kill_on_timeout, S) ->
  {noreply, kill_on_timeout(S)};

handle_info(_Message, S) -> {noreply, S}.

%% Server termination
terminate(_Reason, #state{sid = SID}) ->
  io:format("Client mailbox ~w, ~s terminated~n", [self(), SID]),
  ok.

%% Server code update
code_change(_OldVersion, S, _Extra) -> {ok, S}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% add a line to the list
handle_new_line(Line, #state{lines = Lines} = S) ->
  %io:format("Client mailbox ~w, got new line ~w~n", [self(), Line]),
  send_lines_to_listener(S#state{lines = [Line | Lines]}).

%% subscribe listener and send lines if any exist
handle_subscribe(NewListener, #state{listener = Listener} = S) ->
  case Listener of
    L when L == undefined;
           L == NewListener -> % no old listener or it is the same
      ok;
    _ -> % old listener exists, cancel it
      Listener ! cancel
  end,
  send_lines_to_listener(S#state{listener = NewListener}).

send_lines_to_listener(#state{lines = Lines, listener = Listener} = S)
    when (length(Lines) > 0) and (Listener /= undefined) ->
  Listener ! {lines, lists:reverse(Lines), self()},
  S#state{listener = undefined};
send_lines_to_listener(S) ->
  S.

%% empty the list of lines
handle_empty_lines(S) ->
  S#state{lines = []}.

set_last_update(S) ->
  S#state{last_update = now()}.

kill_on_timeout(#state{last_update = Then, sid = SID} = S) ->
  case timer:now_diff(now(), Then) of
    T when T > (?mailbox_timeout * 1000) ->
      client_manager:remove_sid(SID),
      exit(normal);
    _ ->
      S
  end.