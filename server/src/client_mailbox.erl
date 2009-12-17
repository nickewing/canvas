-module(client_mailbox).
-author('Nick Ewing <nick@nickewing.net>').

-behaviour(gen_server).

-export([
  %% gen_server callbacks
  init/1, handle_call/3, handle_cast/2,
  handle_info/2, terminate/2, code_change/3,
  %% Server interface
  start_new/4, empty_lines/1, subscribe/2, unsubscribe/2, send_line/2,
  %% Testing
  client_manager_stub/1, listener_stub/1
]).

-include("canvas.hrl").
-include_lib("eunit/include/eunit.hrl").

%% internal server state
-record(state, {sid, lines, last_update, listener, cm, timeout}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% External API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Server interface
%%%%%%%%%%%%%%%%%%%%

%% @doc Start a new mailbox
start_new(CM, SID, TimeoutInt, Timeout) ->
  gen_server:start_link(?MODULE, [CM, SID, TimeoutInt, Timeout], []).
%% @doc Send async message
cast(MB, M) ->
  gen_server:cast(MB, M).
%% @doc Empty mailbox lines
empty_lines(MB) ->
  cast(MB, empty_lines).
%% @doc Subscribe listener to mailbox
subscribe(MB, Pid) ->
  cast(MB, {subscribe, Pid}).
%% @doc Unsubscribe listener from mailbox
unsubscribe(MB, Pid) ->
  cast(MB, {unsubscribe, Pid}).
%% @doc Send a line to a mailbox
send_line(MB, Line) ->
  cast(MB, Line).

%%% gen_server Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%

%% Called when a connection is made to the server

%% @doc Initiate server
init([CM, SID, TimeoutInt, Timeout]) ->
  timer:send_interval(TimeoutInt, kill_on_timeout),
  io:format("~w Client mailbox, ~s started.~n", [self(), SID]),
  {ok, #state{sid = SID,
              lines = [],
              last_update = now(),
              cm = CM,
              timeout = Timeout}}.

%% @doc Handle sync. message
handle_call(_Message, _From, S) ->
  {reply, error, S}.

%% @doc Handle async. message
handle_cast(Message, S) ->
  updated_cast(Message, set_last_update(S)).

%% @doc Handle other messages
handle_info(kill_on_timeout, #state{last_update = Then, timeout = Timeout,
                                    sid = SID, cm = CM} = S) ->
  S1 =  case timer:now_diff(now(), Then) of
          T when T > (Timeout * 1000) ->
            client_manager:remove_sid(CM, SID),
            exit(normal);
          _ ->
            S
        end,
  {noreply, S1};
handle_info(_Message, S) ->
  {noreply, S}.

%% @doc Server termination
terminate(_Reason, #state{sid = SID}) ->
  io:format("~w Client mailbox, ~s terminated~n", [self(), SID]),
  ok.

%% @doc Server code update
code_change(_OldVersion, S, _Extra) ->
  {ok, S}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% add a line to the list
updated_cast(#line{} = Line, #state{lines = Lines} = S) ->
  %io:format("~w Client mailbox, got new line ~w~n", [self(), Line]),
  {noreply, send_lines_to_listener(S#state{lines = [Line | Lines]})};
%% subscribe listener and send lines if any exist
updated_cast({subscribe, NewListener}, #state{listener = Listener} = S) ->
  case Listener of
    L when L == undefined;
           L == NewListener -> % no old listener or it is the same
      ok;
    _ -> % old listener exists, tell it it was unsubscribed
      %io:format("~w forcefully unsubscribed ~w~n", [self(), Listener]),
      Listener ! unsubscribed
  end,
  %io:format("~w subscribed ~w~n", [self(), NewListener]),
  {noreply, send_lines_to_listener(S#state{listener = NewListener})};
%% unsubscribe listener
updated_cast({unsubscribe, Pid}, #state{listener = Listener} = S)
    when (Pid == Listener) ->
  %io:format("~w unsubscribed ~w~n", [self(), Listener]),
  {noreply, S#state{listener = undefined}};
updated_cast({unsubscribe, _Pid}, S) ->
  {noreply, S};
%% empty the list of lines
updated_cast(empty_lines, S) ->
  %io:format("~w was emptied~n", [self()]),
  {noreply, S#state{lines = []}};
%% unknown message received
updated_cast(_Message, S) ->
  {noreply, S}.

%% send line list to the listener
send_lines_to_listener(#state{lines = Lines, listener = Listener} = S)
    when (length(Lines) > 0) and (Listener /= undefined) ->
  %io:format("~w sending lines to ~w~n", [self(), Listener]),
  Listener ! {lines, lists:reverse(Lines), self()},
  S#state{listener = undefined};
send_lines_to_listener(#state{lines = Lines} = S)
    when (length(Lines) > 0) ->
  %io:format("~w no listener for lines~n", [self()]),
  S;
send_lines_to_listener(S) ->
  S.

%% set last_update
set_last_update(S) ->
  S#state{last_update = now()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Tests all module functions except timeout
use_test_() ->
  {setup,
    fun() -> ok end,
    ?_test(begin
      % setup
      CM        = spawn(?MODULE, client_manager_stub, [self()]),
      {ok, MB}  = start_new(CM, "abc", 10000, 10000),
      L         = #line{points = [0,1,2,3]},
      Lines     = [L],
      Listener1 = spawn(?MODULE, listener_stub, [self()]),
      Listener2 = spawn(?MODULE, listener_stub, [self()]),
      
      % send a line, subscribe and get line
      client_mailbox:send_line(MB, L),
      client_mailbox:subscribe(MB, Listener1),
      receive
        {listener_stub_got_lines, MBR1, LinesR1} ->
          ?assertEqual(MB, MBR1),
          ?assertEqual(Lines, LinesR1)
      after 10 ->
        erlang:error({timeout, "Listener did not get lines."})
      end,
      client_mailbox:empty_lines(MB),
      
      % subscribe, send a line and get line
      client_mailbox:subscribe(MB, Listener1),
      client_mailbox:send_line(MB, L),
      receive
        {listener_stub_got_lines, MBR2, LinesR2} ->          
          ?assertEqual(MB, MBR2),
          ?assertEqual(Lines, LinesR2)
      after 10 ->
        erlang:error({timeout, "Listener did not get lines."})
      end,
      client_mailbox:empty_lines(MB),
      
      % test unsubscription of first listener on event of second subscription
      client_mailbox:subscribe(MB, Listener1),
      client_mailbox:subscribe(MB, Listener2),
      receive
        {listener_stub_unsubscribed, UnSubPid} ->
          ?assertEqual(Listener1, UnSubPid)
      after 10 ->
        erlang:error({timeout, "Listener was not unsubscribed."})
      end,
      
      client_mailbox:unsubscribe(MB, Listener1),
      client_mailbox:unsubscribe(MB, Listener2),
      
      % cleanup
      exit(MB, normal),
      exit(Listener1, normal),
      exit(Listener2, normal),
      exit(CM, normal)
    end)}.

%% Tests mailbox timeout
timeout_test_() ->
  {setup,
    fun() -> ok end,
    ?_test(begin
      % test timeout and death of mailboxes
      CM         = spawn(?MODULE, client_manager_stub, [self()]),
      {ok, MB}   = start_new(CM, "cba", 20, 30),
      
      receive
        {client_manager_stub_remove_sid, SID} ->
          ?assertEqual(SID, "cba")
      after 100 ->
        erlang:error({timeout, "Client manager did not receive remove_sid."})
      end,
      
      % wait a bit and make sure its dead
      timer:sleep(100),
      ?assertNot(is_process_alive(MB)),
      
      % cleanup
      exit(CM, normal)
    end)}.

%% Stub to imitate client_manager for testing
client_manager_stub(ParentPid) ->
  receive
    {'$gen_cast', {remove_sid, SID}} ->
      ParentPid ! {client_manager_stub_remove_sid, SID};
    A ->
      ?debugVal(A)
  end,
  client_manager_stub(ParentPid).

%% Stub to imitate a mailbox listener process for testing
listener_stub(ParentPid) ->
  receive
    {lines, Lines, MB} ->
      ParentPid ! {listener_stub_got_lines, MB, Lines};
    unsubscribed ->
      ParentPid ! {listener_stub_unsubscribed, self()}
  end,
  listener_stub(ParentPid).