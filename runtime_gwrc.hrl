-define(become(F), runtime_gwr:become(F)).
-define(self(), runtime_gwr:usr_self()).
-define(neighbor(N), runtime_gwr:neighbor(N)).

% overrided functions
-define(send(Dest, Msg), runtime_gwrc:send(Dest, Msg)).
-define(new(F), runtime_gwrc:new(F)).
-define(new_group(Fs), runtime_gwrc:new_group(Fs)).

% cotnext
-define(set_context(Context), runtime_gwrc:set_context(Context)).
-define(self_context(), runtime_gwrc:self_context()).

% for experiments
-define(send_delay(Dest, Msg, Delay), runtime_gwrc:send_delay(Dest, Msg, Delay)).
-define(send_context(Dest, Context), runtime_gwrc:send_context(Dest, Context)).
-define(send_context_delay(Dest, Context, Delay), runtime_gwrc:send_context_delay(Dest, Context, Delay)).
