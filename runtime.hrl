-define(send(Dest, Msg), runtime:send(Dest, Msg)).
-define(new(F), runtime:new(F)).
-define(new_group(Fs), runtime:new_group(Fs)).
-define(become(F), runtime:become(F)).
-define(self(), runtime:usr_self()).
-define(neighbor(N), runtime:neighbor(N)).

% for experiments
-define(send_delay(Dest, Msg, Delay), runtime:send_delay(Dest, Msg, Delay)).
