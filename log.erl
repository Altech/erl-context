-module(log).
-compile(export_all).

%% log for an context-aware actor

%% Log ::= [{Message ID, Message(including Extension), {Context, Function(Behavior)}, {Context, Function(Behavior)}, [{Influenced Actor, Message ID}]}]
%% Second of tuple is state before message-processing.
%% Third of tuple is state after message-processing.

new() ->
    [].

logBefore(L, ID, M, C, F) ->
    [{ID, M, {C, F}}|L].

logAfter([{ID, M, Prev}|L], C, F, Ms) ->
    [{ID, M, Prev, {C, F}, Ms}|L].

messageID(E) ->
    element(1, E).

message(E) ->
    element(2, E).

sentMessages(E) ->
    element(5, E).

sentMessageIDAndDestNumbers(E) ->
    [{N, ID} || {{N, _}, {ID, Msg}} <- element(5, E)].

beforeContext(E) ->
    {C, F} = element(3, E),
    C.

beforeFunction(E) ->
    {C, F} = element(3, E),
    F.

afterContext(E) ->
    {C, F} = element(4, E),
    C.

afterFunction(E) ->
    {C, F} = element(4, E),
    F.

lookup(ID, L) ->
    hd([E || E <- L, messageID(E) == ID]).

lookupWithIndex(ID, L) ->
    I = length(lists:takewhile(fun(E)-> messageID(E) /= ID end, L)),
    {lists:nth(I+1, L), I}.
