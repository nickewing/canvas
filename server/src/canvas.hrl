%% @author Nick Ewing <nick@nickewing.net>
%% @copyright 2009 Nick Ewing.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Server Configuration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Port to run server on
-define(server_port, 8000).

% Time to wait until timing out on /update request
-define(update_request_timeout, 20000). % 20 seconds

% Client mailbox inactivity timeout
-define(mb_timeout, 300000). % 5 minutes
% Interval to check for mailbox timeout
-define(mb_timeout_interval, 60000). % 1 minute

% When when no path is given, redirect here
-define(default_www_path, "/client/index.html").

%% Postgres database connection settings
-define(ls_host,  "localhost").
-define(ls_db,    "canvas").
-define(ls_user,  "canvas").
-define(ls_pass,  "").

%% Size of a canvas tile
-define(tile_size, 500).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Records
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% User information
-record(user, {ip}).
% Tile record information
-record(tile, {id, x, y, last_painted, box}).
% Line information
-record(line, {id, points, size, color, box, time, user}).
% Server state
-record(s_state, {cm}).