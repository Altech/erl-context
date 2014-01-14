-module(log).
-compile(export_all).

%% log for an context-aware actor

%% Log ::= [{Message ID, Message(for debug), {Context, Function(Behavior)}, {Context, Function(Behavior)}, [{Influenced Actor, Message ID}]}]
%% Second of tuple is state before message-processing.
%% Third of tuple is state after message-processing.

new() ->
    [].

logBefore(L, ID, M, C, F) ->
    [{ID, M, {C, F}}|L].

logAfter([{ID, M, Prev}|L], C, F, Ms) ->
    [{ID, M, Prev, {C, F}, Ms}|L].

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
