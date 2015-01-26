-define(send(Dest, Msg), runtime_gwr:send(Dest, Msg)).
-define(new(F), runtime_gwr:new(F)).
-define(new_group(Fs), runtime_gwr:new_group(Fs)).
-define(become(F), runtime_gwr:become(F)).
-define(self(), runtime_gwr:usr_self()).
-define(neighbor(N), runtime_gwr:neighbor(N)).

% for experiments
-define(send_delay(Dest, Msg, Delay), runtime_gwr:send_delay(Dest, Msg, Delay)).
