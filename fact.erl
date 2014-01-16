-module(fact).
-compile([export_all]).

%% -include("runtime.hrl").
-include("runtime_ctx_opt.hrl").

printerG(X) -> io:format("~p~n",[X]).

factG({N, Cont}) ->
    if N == 0 -> ?send(Cont, 1);
       true   -> ?send(?self(), 
		       {N-1, ?new(fun(V) -> ?send(Cont, N * V) end)})
    end.

start() ->
    PrinterG = ?new_group([fun fact:printerG/1]),
    FactG = ?new_group([fun fact:factG/1]),
    [Printer, Fact] = [{1, PrinterG}, {1, FactG}],
    ?send(Fact, {10, Printer}),
    FactG.
