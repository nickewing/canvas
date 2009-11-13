%%% Spatial functions

-module(spatial).
-author('Nicholas E. Ewing <nick@nickewing.net>').

-compile(export_all).
-include("spatial.hrl").

%%% box functions

%% build a bounding box for a list of coords
find_points_box(Points) ->
  [X, Y, X1, Y1] = lists:foldl(
    fun find_points_box/2,
    [unset, unset, unset, unset],
    Points
  ),
  #box{x = X, y = Y, x1 = X1, y1 = Y1}.
find_points_box({X, Y}, [Xo, Yo, X1o, Y1o]) ->
  [
    util:min_with_unset(Xo,  X),
    util:min_with_unset(Yo,  Y),
    util:max_with_unset(X1o, X),
    util:max_with_unset(Y1o, Y)
  ].

%% determines if point is located on a box
point_on_box({PX, PY}, #box{x = X0, y = Y0, x1 = X1, y1 = Y1}) ->
  ((X0 =< PX) and (PX =< X1)) and ((Y0 =< PY) and (PY =< Y1)).

%% determines if two boxes intersect
boxes_intersect(B0, B1) ->
  box_on_box(B0, B1) or box_on_box(B1, B0).

%% determines if b0 is on b1, but not vice versa
box_on_box(#box{x = X0, y = Y0, x1 = X1, y1 = Y1} = _B0, B1) ->
  point_on_box({X0, Y0}, B1) or % top left corner of b0 on b1
  point_on_box({X1, Y1}, B1) or % bottom right corner of b0 on b1
  point_on_box({X0, Y1}, B1) or % bottom left corner of b0 on b1
  point_on_box({X1, Y0}, B1).   % top right corner of b0 on b1

%% Convert a string of format #,#,#,# to box
comma_str_to_box(Str) when is_list(Str)->
  try
    [X, Y, X1, Y1] = lists:map(fun (S) ->
      case util:str_to_num(S) of
        {ok, X}           -> X;
        {error, _Err} = E -> throw(E)
      end
    end, string:tokens(Str, ",")),
    {ok, #box{x = X, y = Y, x1 = X1, y1 = Y1}}
  catch
    {error, _Err} = E -> E
  end;
comma_str_to_box(_) ->
  {error, "Not a string"}.