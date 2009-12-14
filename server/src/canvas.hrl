
-record(user, {ip}).
-record(tile, {box, time}).

-record(line, {points, size, color, box, time, user}).

-record(s_state, {cm, ls}).

-define(server_port, 8000).

-define(line_wait_timeout, 20000). % 20 seconds

-define(mailbox_timeout, 300000). % 5 minutes
-define(mailbox_timeout_interval, 60000). % 1 minute

%% Postgres db info
-define(ls_host,  "localhost").
-define(ls_db,    "canvas").
-define(ls_user,  "canvas").
-define(ls_pass,  "").