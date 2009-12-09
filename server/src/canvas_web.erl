%% @doc Web server for canvas.

-module(canvas_web).
-author('Nicholas E. Ewing <nick@nickewing.net>').

-export([start/1, stop/0, loop/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% External API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start(Options) ->
  client_manager:start(),
  {DocRoot, Options1} = get_option(docroot, Options),
  Loop =  fun (Req) ->
            ?MODULE:loop(Req, DocRoot)
          end,
  mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
  mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
  case Req:get(method) of
    Method when Method =:= 'GET';
                Method =:= 'HEAD';
                Method =:= 'POST' ->
      case request_controller:route_request(Req) of
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
%%% External API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_option(Option, Options) ->
  {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.