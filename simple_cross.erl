-module(simple_cross).
-compile(export_all).

%% -include("runtime.hrl").
-include("runtime_ctx.hrl").

%% Simple application for cross-context messages type (a)
start([Type]) ->
    G = ?new_group([fun simple_cross:apO/1, fun simple_cross:apA/1, fun simple_cross:apB/1]),
    [O, A, B] = [{1, G}, {2, G}, {3, G}],
    ?send(O, {start, {A, B}, Type}),
    if Type == b -> timer:sleep(500); true -> nil end,
    ?send(A, {start, B, Type}),
    G.

apO(Msg) ->
    case Msg of
	{start, {A, B}, Type} ->
	    if Type == a ->
		    ?send_context_delay(A, context:new(new_context_observed_by_O), 2000),
		    ?send_context(     B, context:new(new_context_observed_by_O));
	       Type == b ->
		    ?send_context(     A, context:new(new_context_observed_by_O)),
		    ?send_context_delay(B, context:new(new_context_observed_by_O), 2000)
	    end;
	_ ->
	    io:format("apO received unexpected ~p~n", [Msg])
    end.

apA(Msg) -> 
    case Msg of 
	{start, B, Type} ->
	    io:format("A: send hello to B with context of ~p~n",[context:value(get(context))]),
	    if Type == a ->
		    ?send_delay(B, hello, 2000);
	       Type == b ->
		    ?send_delay(B, hello, 1000)
	    end
    end.

apB(Msg) -> 
    case Msg of 
	hello -> io:format("B: receive hello in context of ~p~n",[context:value(get(context))]);
	_ -> io:format("B: receive ~p~n", [Msg])
    end.

