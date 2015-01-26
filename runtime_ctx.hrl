-define(change_behavior(F, Self), runtime:change_behavior(F, Self)).
-define(self(), runtime:usr_self()).
-define(neighbor(N), runtime:neighbor(N)).

% overrided functions
-define(send(Dest, Msg), runtime_ctx:send(Dest, Msg)).
-define(new(F), runtime_ctx:new(F)).
-define(new_group(Fs), runtime_ctx:new_group(Fs)).

% for experiments
-define(send_delay(Dest, Msg, Delay), runtime_ctx:send_delay(Dest, Msg, Delay)).
-define(send_context(Dest, Context), runtime_ctx:send_context(Dest, Context)).
-define(send_context_delay(Dest, Context, Delay), runtime_ctx:send_context_delay(Dest, Context, Delay)).
