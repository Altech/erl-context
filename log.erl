-module(log).
-compile(export_all).

%% log for an context-aware actor

%% Log ::= [{Message ID, Message(including Extension), {Context, Function(Behavior)}, {Context, Function(Behavior)}, [{Influenced Actor, Message ID}]}]
%% Second of tuple is state before message-processing.
%% Third of tuple is state after message-processing.

new() ->
    [].

log_before(L, ID, M, C, F) ->
    [{ID, M, {C, F}}|L].

log_after([{ID, M, Prev}|L], C, F, Ms) ->
    [{ID, M, Prev, {C, F}, Ms}|L].

message_ID(E) ->
    element(1, E).

message(E) ->
    element(2, E).

sent_messages(E) ->
    element(5, E).

sent_message_ID_and_dest_numbers(E) ->
    [{N, ID} || {{N, _}, {ID, Msg}} <- element(5, E)].

before_context(E) ->
    {C, F} = element(3, E),
    C.

before_function(E) ->
    {C, F} = element(3, E),
    F.

after_context(E) ->
    {C, F} = element(4, E),
    C.

after_function(E) ->
    {C, F} = element(4, E),
    F.

processing(E)  ->
    case tuple_size(E) of
        3 -> true;
        5 -> false
    end.

lookup(ID, L) ->
    Ls = [E || E <- L, message_ID(E) == ID],
    case Ls of
        [E|_] -> E;
        _ -> not_found
    end.

lookup_with_index(ID, L) ->
    I = length(lists:takewhile(fun(E)-> message_ID(E) /= ID end, L)),
    {lists:nth(I+1, L), I}.
