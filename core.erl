-module(core).
-export([new/1, become/1, put/1, self/0, send/2]).
-compile({no_auto_import,[self/0, send/2]}).

new(Fun) ->
    spawn(fun() ->
		  receive
		      X -> Fun(X)
		  end
	  end).

become(Fun) ->
    receive
	X -> Fun(X)
    end.

% Actually, use built-in self/0
self() -> (fun erlang:self/0)().

% Actually, use built-in send/2('!' operator)
send(Pid, Msg) -> Pid ! Msg.

put(Msg) ->
    self() ! Msg.
