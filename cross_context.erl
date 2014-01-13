-module(cross_context).
-compile(export_all).

-include("runtime.hrl").
-include("runtime_ctx_opt.hrl").

%% Simple application for cross-context messages type (a)
start(Type) ->
    G = ?newCtxG([fun cross_context:apO/3, fun cross_context:apA/3, fun cross_context:apB/3]),
    [O, A, B] = [{1, G}, {2, G}, {3, G}],
    ?send(O, {start, {A, B}, Type}),
    if Type == b -> timer:sleep(500); true -> nil end,
    ?send(A, {start, B, Type}),
    G.

apO(Msg, Self, Context) ->
    case Msg of
	{start, {A, B}, Type} ->
	    io:format("O: receive ~p~n", [{start, {A, B}}]),	    
	    if Type == a ->
		    ?sendDelay(A, context:new(new_context_observed_by_O), 2000),
		    ?send(B, context:new(new_context_observed_by_O));
	       Type == b ->
		    ?send(A, context:new(new_context_observed_by_O)),
		    ?sendDelay(B, context:new(new_context_observed_by_O), 2000)
	    end;
	_ ->
	    io:format("apO received unexpected ~p~n", [Msg])
    end.

apA(Msg, Self, Context) -> 
    case Msg of 
	{start, B, Type} ->
	    io:format("A: send hello to B with context of ~p~n",[context:value(Context)]),
	    if Type == a ->
		    ?sendDelay(B, hello, Context, 2000);
	       Type == b ->
		    ?sendDelay(B, hello, Context, 1000)
	    end
    end.

apB(Msg, Self, Context) -> 
    case Msg of 
	hello -> io:format("B: receive hello in context of ~p~n",[context:value(Context)]);
	_ -> io:format("B: receive ~p~n", [Msg])
    end.

