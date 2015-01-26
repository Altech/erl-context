-module(cross_simple).
-compile(export_all).

%% -include("runtime_gwr.hrl").
-include("runtime_gwrc.hrl").

%% Simple application for cross-context messages type (a)
main([TypeStr]) ->
    Type = list_to_atom(TypeStr),
    G = ?new_group([fun cross_simple:apO/1, fun cross_simple:apA/1, fun cross_simple:apB/1]),
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

