-define(send(Dest, Msg, Ctx), runtime_ctx:send(Dest, Msg, Ctx)).
-define(newCtxG(Fs), runtime_ctx:newCtxG(Fs)).
-define(sendDelay(Dest, Msg, Delay), runtime_ctx:sendDelay(Dest, Msg, Delay)).
-define(sendDelay(Dest, Msg, Ctx, Delay), runtime_ctx:sendDelay(Dest, Msg, Ctx, Delay)).

