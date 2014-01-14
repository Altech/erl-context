-module(sb).
-compile(export_all).

start(S) ->
    put(stateOfGroupActor, S).

showLog(L) ->
    lists:map(fun({ID, M, Prev, After, Ms}) ->
		      io:format(" ID: ~p~n", [ID]),
		      io:format(" Message: ~p~n",[M]),
		      io:format(" State(Prev):  ~p~n", [Prev]),
		      io:format(" State(After): ~p~n", [After]),
		      if Ms /= [] -> io:format(" Sent Messages: ~p~n", [lists:reverse(Ms)]); true -> nil end,
		      io:format(" -------------------------------------------------------~n",[])
	      end, lists:reverse(L)),
    nil.

show(N) ->
    io:format("- [~pth of logs] ----------------------------------------~n",[N]),
    showLog(nth(N, log())).

showAll() ->
    Logs = log(),
    lists:map(fun({L, N}) -> 
		      io:format("- [~pth of logs] ----------------------------------------~n",[N]),
		      showLog(L)
	      end, lists:zip(Logs, lists:seq(1, length(Logs)))),
    nil.

log() ->
    case get(stateOfGroupActor) of
	{Qs, Fs, Ss, Cs, Ls} -> Ls;
	[Ls] -> [Ls];
	Else -> io:format("unknown state(~p)~n",[Else]), [Else]
    end.

% utils
nth(N, [H|T]) ->
    case N of
	1 -> H;
	N when N > 1 -> nth(N-1, T)
    end.
