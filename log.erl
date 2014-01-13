-module(log).
-compile(export_all).

%% log for an context-aware actor

%% Log ::= [{Message, {Context, Function(Behavior)}, {Context, Function(Behavior)}, [Influenced Actor]}]
%% Second of tuple is state before message-processing.
%% Third of tuple is state after message-processing.

new() ->
    [].

logBefore(L, M, C, F) ->
    [{M, {C, F}}|L].

logAfter([{M, Prev}|L], C, F) ->
    [{M, Prev, {C, F}, []}|L].

%% cancel(L, N, MetaCtx) ->
%%     case L of
%% 	[] -> ok;
%% 	[{M, C, F, As}] ->
%% 	    core:become(metaCtx())
%% 	[{M, C, F, As}|T] -> 
%% 	    % send to cancel message to influenced actors
%% 	    cancel(T, MetaCtx)
%%     end.

%% cancelActors() ->
%%     nil.
