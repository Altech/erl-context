-module(fact).
-compile([export_all]).

%% -include("runtime_base.hrl").
%% -include("runtime_gwr.hrl").
-include("runtime_gwrc.hrl").

printer(X) -> io:format("~p~n",[X]).

fact({N, Cont}) ->
    if N == 0 -> ?send(Cont, 1);
       true   -> ?send(?self(), 
		       {N-1, ?new(fun(V) -> ?send(Cont, N * V) end)})
    end.

start() ->
    PrinterG = ?new_group([fun fact:printer/1]),
    FactG = ?new_group([fun fact:fact/1]),
    [Printer, Fact] = [{1, PrinterG}, {1, FactG}],
    ?send(Fact, {10, Printer}),
    FactG.

%% start_core() ->
%%     Printer = ?new(fun fact:printer/1),
%%     Fact = ?new(fun fact:fact/1),
%%     ?send(Fact, {10, Printer}),
%%     Fact.
