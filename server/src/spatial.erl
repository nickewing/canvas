%% @author Nick Ewing <nick@nickewing.net>
%% @copyright 2009 Nick Ewing.

%% @doc Spatial functions

-module(spatial).
-author('Nicholas E. Ewing <nick@nickewing.net>').

-export([
  points_box/1,
  boxes_box/1,
  boxes_box/2,
  point_on_box/3,
  boxes_intersect/2
]).

-include("spatial.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% External API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc build a bounding box for a list of coords
points_box([X|[Y|T]] = L) when (length(L) > 0) and (length(L) rem 2 == 0) ->
  {[Xr, Yr, X1r, Y1r], _} = lists:foldl(fun points_box/2, {[X, Y, X, Y], x}, T),
  #box{x = Xr, y = Yr, x1 = X1r, y1 = Y1r}.

%% @doc Calculates the surrounding box for a list of boxes
boxes_box(Boxes) ->
  util:reducel(fun boxes_box/2, Boxes).

%% @doc Calculates the surrounding box for two boxes
boxes_box(#box{x = X0, y = Y0, x1 = X01, y1 = Y01},
          #box{x = X1, y = Y1, x1 = X11, y1 = Y11}) ->
  #box{
    x  = lists:min([X0, X01, X1, X11]),
    y  = lists:min([Y0, Y01, Y1, Y11]),
    x1 = lists:max([X0, X01, X1, X11]),
    y1 = lists:max([Y0, Y01, Y1, Y11])
  }.

%% @doc Determines if point is located on a box
point_on_box(PX, PY, #box{x = X0, y = Y0, x1 = X1, y1 = Y1}) ->
  ((X0 =< PX) and (PX =< X1)) and ((Y0 =< PY) and (PY =< Y1)).

%% @doc Determines if two boxes intersect
boxes_intersect(B0, B1) ->
  box_on_box(B0, B1) or box_on_box(B1, B0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Folder for points_box/1
points_box(V, {[Xo, Yo, X1o, Y1o], Type}) ->
  case Type of
    x -> {[erlang:min(Xo,  V), Yo, erlang:max(X1o, V), Y1o], y};
    y -> {[Xo, erlang:min(Yo,  V), X1o, erlang:max(Y1o, V)], x}
  end.

%% Determines if b0 is on b1, but not vice versa
box_on_box(#box{x = X0, y = Y0, x1 = X1, y1 = Y1} = _B0, B1) ->
  point_on_box(X0, Y0, B1) or % top left corner of b0 on b1
  point_on_box(X1, Y1, B1) or % bottom right corner of b0 on b1
  point_on_box(X0, Y1, B1) or % bottom left corner of b0 on b1
  point_on_box(X1, Y0, B1).   % top right corner of b0 on b1

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

points_box_test() ->
  [
    ?assertError(function_clause, points_box([])),
    ?assertError(function_clause, points_box([1])),
    ?assertError(function_clause, points_box([1, 4.3, 4])),
    ?assert(
      points_box([4, 5, -23, 5, 50, 23.1, 4, 5])
        =:= #box{x = -23, y = 5, x1 = 50, y1 = 23.1}
    ),
    ?assert(
      points_box([0, 0, 0, 0, 0, 0, 0, 0])
        =:= #box{x = 0, y = 0, x1 = 0, y1 = 0}
    )
  ].

boxes_box_test() ->
  B1 = #box{x = -6, y = -5, x1 = 0, y1 = 0},
  B2 = #box{x = -12, y = -10.2, x1 = 14.3, y1 = 10.3},
  B3 = #box{x = 0.2, y = 0.3, x1 = 100, y1 = 2000},
  [
    ?assert(
      boxes_box([B1, B2, B3]) =:= #box{x = -12, y = -10.2, x1 = 100, y1 = 2000}
    ),
    ?assert(
      boxes_box([B1]) =:= #box{x = -6, y = -5, x1 = 0, y1 = 0}
    ),
    ?assert(
      boxes_box(B1, B2) =:= #box{x = -12, y = -10.2, x1 = 14.3, y1 = 10.3}
    )
  ].

point_on_box_test() ->
  [
    % on corner of box
    ?assert(point_on_box(0, 0, #box{x = -6, y = -5, x1 = 0, y1 = 0})),
    % completely inside box
    ?assert(point_on_box(0, 0, #box{x = -6, y = -5, x1 = 10, y1 = 10})),
    % not inside box
    ?assertNot(point_on_box(-20.0, 0, #box{x = -6, y = -5, x1 = 10, y1 = 10})),
    % not inside box
    ?assertNot(point_on_box(0, -20, #box{x = -6, y = -5, x1 = 10, y1 = 10})),
    % not inside box
    ?assertNot(point_on_box(-30, -20, #box{x = -6, y = -5, x1 = 10, y1 = 10}))
  ].

%% Effectively tests both boxes_intersect and box_on_box
boxes_intersect_test() ->
  [
    % Fully overlapping
    ?assert(
      boxes_intersect(
        #box{x = -12, y = -10.2, x1 = 14.3, y1 = 10.3},
        #box{x = -6, y = -5, x1 = 0, y1 = 0}
      )
    ),
    % Fully overlapping reversed
    ?assert(
      boxes_intersect(
        #box{x = -6, y = -5, x1 = 0, y1 = 0},
        #box{x = -12, y = -10.2, x1 = 14.3, y1 = 10.3}
      )
    ),
    % Partly intersecting
    ?assert(
      boxes_intersect(
        #box{x = -12, y = -10.2, x1 = 14.3, y1 = 10.3},
        #box{x = 0.2, y = 0.3, x1 = 100, y1 = 2000}
      )
    ),
    % Partly intersecting reversed
    ?assert(
      boxes_intersect(
        #box{x = 0.2, y = 0.3, x1 = 100, y1 = 2000},
        #box{x = -12, y = -10.2, x1 = 14.3, y1 = 10.3}
      )
    ),
    % not intersecting
    ?assertNot(
      boxes_intersect(
        #box{x = -9, y = -10.2, x1 = 14.3, y1 = 10.3},
        #box{x = 40, y = 40, x1 = 80, y1 = 80}
      )
    )
  ].