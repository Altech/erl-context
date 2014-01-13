-define(change_behavior(F, Self), runtime:change_behavior(F, Self)).
-define(self(), runtime:usr_self()).

% overrided functions
-define(send(Dest, Msg), runtime_ctx_opt:send(Dest, Msg)).
-define(new(F), runtime_ctx_opt:new(F)).
-define(newG(Fs), runtime_ctx_opt:newG(Fs)).

% for experiments
-define(sendDelay(Dest, Msg, Delay), runtime_ctx_opt:sendDelay(Dest, Msg, Delay)).
-define(sendContext(Dest, Context), runtime_ctx_opt:sendContext(Dest, Context)).
-define(sendContextDelay(Dest, Context, Delay), runtime_ctx_opt:sendContextDelay(Dest, Context, Delay)).
