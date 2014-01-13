-module(cross).
-compile(export_all).

%% -include("runtime.hrl").
-include("runtime_ctx_opt.hrl").

%% Simple application for cross-context messages type (a)
start([Type]) ->
    G = ?newG([fun cross:apO/1, fun cross:apA/1, fun cross:apB/1]),
    [O, A, B] = [{1, G}, {2, G}, {3, G}],
    ?send(O, {start, {A, B}, Type}),
    if Type == b -> timer:sleep(500); true -> nil end,
    ?send(A, {start, B, Type}),
    G.

apO(Msg) ->
    case Msg of
	{start, {A, B}, Type} ->
	    if Type == a ->
		    ?sendContextDelay(A, context:new(new_context_observed_by_O), 2000),
		    ?sendContext(     B, context:new(new_context_observed_by_O));
	       Type == b ->
		    ?sendContext(     A, context:new(new_context_observed_by_O)),
		    ?sendContextDelay(B, context:new(new_context_observed_by_O), 2000)
	    end;
	_ ->
	    io:format("apO received unexpected ~p~n", [Msg])
    end.

apA(Msg) -> 
    case Msg of 
	{start, B, Type} ->
	    io:format("A: send hello to B with context of ~p~n",[context:value(get(context))]),
	    if Type == a ->
		    ?sendDelay(B, hello, 2000);
	       Type == b ->
		    ?sendDelay(B, hello, 1000)
	    end
    end.

apB(Msg) -> 
    case Msg of 
	hello -> io:format("B: receive hello in context of ~p~n",[context:value(get(context))]);
	_ -> io:format("B: receive ~p~n", [Msg])
    end.

