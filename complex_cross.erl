-module(complex_cross).
-compile(export_all).

%% -include("runtime.hrl").
-include("runtime_ctx_opt.hrl").

%% Simple application for cross-context messages type (a)
start() ->
    G = ?newG([fun complex_cross:a1/1, fun complex_cross:a2/1, fun complex_cross:a3/1, fun complex_cross:aO/1]),
    O = {4, G},
    ?send(O, start),
    G.

context1() ->
    {'$context', {context1, {0,0,1}}}.

context2() ->
    {'$context', {context2, {0,0,2}}}.

aO(start) ->
    put(context, context1()), % [TODO] replace synchronous change_self_context
    ?send(?neighbor(1), first_from_observer),
    put(context, context2()),
    ?send(?neighbor(3), first_from_observer),
    ?sendDelay(?neighbor(1), start_from_observer, 500),
    ?sendDelay(?neighbor(3), start_from_observer, 500),
    % send cross-context message!
    put(context, context1()),
    ?sendDelay(?neighbor(1), yheeaaaa, 2200).

a1(Msg) ->
    case Msg of
	first_from_observer ->
	    p("get first message from observer~n");
	start_from_observer ->
	    p("start~n"),
	    ?send(?neighbor(2), yhaa);
	yhaaa ->
	    p("get yhaaa from A2~n");
	yheeaaaa ->
	    p("get yheeaaaa(cross-context message)~n");
	_ ->
	    p("received unexpected ~p~n", [Msg])
    end.

a2(Msg) -> 
    case Msg of
	yhaa ->
	    p("get yhaa from A1~n"),
	    ?sendDelay(?neighbor(1), yhaaa, 500);
	inst1 ->
	    p("get instruction of 1 and replying~n"),
	    ?sendDelay(?neighbor(3), inst1_reply, 600);
	inst2 ->
	    p("get instruction of 2~n");
	_ ->
	    p("received unexpected ~p~n", [Msg])
    end.

a3(Msg) -> 
    case Msg of
	first_from_observer -> 
	    p("get first message from observer~n");
	start_from_observer ->
	    p("start send two instrunction to A2~n"),
	    ?sendDelay(?neighbor(2), inst1, 520),
	    ?sendDelay(?neighbor(2), inst2, 1020);
	inst1_reply ->
	    p("get reply of instruction of 1~n")
    end.

p(S) ->
    p(S, []).

p(S, Ls) ->
    {N, _} = ?self(),
    io:format(string:concat(io_lib:format("A~p: ",[N]), S), Ls).
