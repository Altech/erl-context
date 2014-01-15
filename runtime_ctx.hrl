-define(change_behavior(F, Self), runtime:change_behavior(F, Self)).
-define(self(), runtime:usr_self()).

% overrided functions
-define(send(Dest, Msg), runtime_ctx:send(Dest, Msg)).
-define(new(F), runtime_ctx:new(F)).
-define(newG(Fs), runtime_ctx:newG(Fs)).

% for experiments
-define(sendDelay(Dest, Msg, Delay), runtime_ctx:sendDelay(Dest, Msg, Delay)).
-define(sendContext(Dest, Context), runtime_ctx:sendContext(Dest, Context)).
-define(sendContextDelay(Dest, Context, Delay), runtime_ctx:sendContextDelay(Dest, Context, Delay)).
