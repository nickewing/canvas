
-record(user, {ip_addr}).
-record(tile, {box, time}).
-record(line, {points, size, color, user}).

-record(s_state, {cm}).

-define(server_port, 8000).

-define(line_wait_timeout, 20000). % 20 seconds

-define(mailbox_timeout, 300000). % 5 minutes
-define(mailbox_timeout_interval, 60000). % 1 minute

%% Postgres db info
-define(ls_store_host,  "localhost").
-define(ls_store_db,    "mochi_test").
-define(ls_store_user,  "mochi_test").
-define(ls_store_pass,  "").