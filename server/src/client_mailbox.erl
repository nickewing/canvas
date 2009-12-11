-module(client_mailbox).
-author('Nick Ewing <nick@nickewing.net>').

-behaviour(gen_server).

-export([
  %% gen_server callbacks
  init/1, handle_call/3, handle_cast/2,
  handle_info/2, terminate/2, code_change/3,
  %% Server interface
  start_new/2, empty_lines/1, subscribe/2, unsubscribe/2, send_line/2
]).

-include("canvas.hrl").

%% internal server state
-record(state, {sid, lines, last_update, listener, cm}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% External API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Server interface
%%%%%%%%%%%%%%%%%%%%

start_new(CM, SID) ->
  gen_server:start_link(?MODULE, [CM, SID], []).
cast(MB, M) ->
  gen_server:cast(MB, M).
empty_lines(MB) ->
  cast(MB, empty_lines).
subscribe(MB, Pid) ->
  cast(MB, {subscribe, Pid}).
unsubscribe(MB, Pid) ->
  cast(MB, {unsubscribe, Pid}).
send_line(MB, Line) ->
  cast(MB, Line).

%%% gen_server Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%

%% Called when a connection is made to the server

init([CM, SID]) ->
  timer:send_interval(?mailbox_timeout_interval, kill_on_timeout),
  io:format("~w Client mailbox, ~s started.~n", [self(), SID]),
  {ok, #state{sid = SID, lines = [], last_update = now(), cm = CM}}.

%% Invoked in response to gen_server:call
%% Update last_update on all calls and then call update_call
handle_call(_Message, _From, S) ->
  {reply, error, S}.

%% Invoked in response to gen_server:cast
handle_cast(Message, S) ->
  updated_cast(Message, set_last_update(S)).

%% Handle other messages
handle_info(kill_on_timeout, #state{last_update=Then, sid=SID, cm=CM} = S) ->
  S1 =  case timer:now_diff(now(), Then) of
          T when T > (?mailbox_timeout * 1000) ->
            client_manager:remove_sid(CM, SID),
            exit(normal);
          _ ->
            S
        end,
  {noreply, S1};
handle_info(_Message, S) ->
  {noreply, S}.

%% Server termination
terminate(_Reason, #state{sid = SID}) ->
  io:format("~w Client mailbox, ~s terminated~n", [self(), SID]),
  ok.

%% Server code update
code_change(_OldVersion, S, _Extra) ->
  {ok, S}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% add a line to the list
updated_cast(#line{} = Line, #state{lines = Lines} = S) ->
  io:format("~w Client mailbox, got new line ~w~n", [self(), Line]),
  {noreply, send_lines_to_listener(S#state{lines = [Line | Lines]})};
%% subscribe listener and send lines if any exist
updated_cast({subscribe, NewListener}, #state{listener = Listener} = S) ->
  case Listener of
    L when L == undefined;
           L == NewListener -> % no old listener or it is the same
      ok;
    _ -> % old listener exists, tell it it was unsubscribed
      io:format("~w forcefully unsubscribed ~w~n", [self(), Listener]),
      Listener ! unsubscribed
  end,
  io:format("~w subscribed ~w~n", [self(), NewListener]),
  {noreply, send_lines_to_listener(S#state{listener = NewListener})};
%% unsubscribe listener
updated_cast({unsubscribe, Pid}, #state{listener = Listener} = S)
    when (Pid == Listener) ->
  io:format("~w unsubscribed ~w~n", [self(), Listener]),
  {noreply, S#state{listener = undefined}};
updated_cast({unsubscribe, _Pid}, S) ->
  {noreply, S};
%% empty the list of lines
updated_cast(empty_lines, S) ->
  io:format("~w was emptied~n", [self()]),
  {noreply, S#state{lines = []}};
%% unknown message received
updated_cast(_Message, S) ->
  {noreply, S}.

%% send line list to the listener
send_lines_to_listener(#state{lines = Lines, listener = Listener} = S)
    when (length(Lines) > 0) and (Listener /= undefined) ->
  io:format("~w sending lines to ~w~n", [self(), Listener]),
  Listener ! {lines, lists:reverse(Lines), self()},
  S#state{listener = undefined};
send_lines_to_listener(#state{lines = Lines} = S)
    when (length(Lines) > 0) ->
  io:format("~w no listener for lines~n", [self()]),
  S;
send_lines_to_listener(S) ->
  S.

%% set last_update
set_last_update(S) ->
  S#state{last_update = now()}.