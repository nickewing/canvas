
-record(user, {ip_addr}).
-record(tile, {box, time}).
-record(line, {points, size, color, user}).


-define(server_port, 8000).

-define(line_wait_timeout, 10000).


-define(mailbox_timeout, 3000000). % 5 minutes
-define(mailbox_timeout_interval, 3000000). % 5 minutes

%% Postgres db info
-define(ls_store_host,  "localhost").
-define(ls_store_db,    "mochi_test").
-define(ls_store_user,  "mochi_test").
-define(ls_store_pass,  "").