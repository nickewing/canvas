%% @doc Web server for canvas.

-module(canvas_web).
-author('Nicholas E. Ewing <nick@nickewing.net>').

-export([start/1, stop/0, loop/3]).

-include("canvas.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% External API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Options) ->
  {ok, CM} = client_manager:start(),
  S = #s_state{cm = CM, ls = line_store:connect()},
  io:format("~p~n", [S]),
  {DocRoot, Options1} = get_option(docroot, Options),
  Loop =  fun (Req) ->
            ?MODULE:loop(Req, S, DocRoot)
          end,
  mochiweb_http:start([
    {max,  1000000}, % max connections
    {name, ?MODULE},
    {loop, Loop}
    | Options1
  ]).

stop() ->
  mochiweb_http:stop(?MODULE).

loop(Req, S, DocRoot) ->
  case Req:get(method) of
    Method when Method =:= 'GET';
                Method =:= 'HEAD';
                Method =:= 'POST' ->
      case request_controller:route_request(Req, S) of
        no_route ->
          "/" ++ Path = Req:get(path),
          Req:serve_file(Path, DocRoot);
        Resp ->
          Resp
      end;
    _ ->
      Req:respond({501, [], []})
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_option(Option, Options) ->
  {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.