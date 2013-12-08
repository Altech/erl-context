-module(runtime).
-export([start/0, restart/0, exec/1, new/1, send/2]).

%%%=========================================================================
%%%  API
%%%=========================================================================

start() ->
    % register, whereis はグローバル変数みたいなもの。これを使ってしまっている。
    MaybePid = whereis(exec),
    if MaybePid == undefined -> register(exec, core:new(fun exec/1));
       true                  -> already_started
    end.

restart() ->
    MaybePid = whereis(exec),
    if MaybePid /= undefined -> exit(MaybePid, kill); true -> true end,
    catch unregister(exec), 
    register(exec, core:new(fun exec/1)).

new(F) ->
    core:new(meta1([], F, dormant)).

send(Dest, Msg) ->
    Dest ! {mesg, Msg}.

put(Msg, Self) ->
    send(Self, Msg).

%% %% This will be called in exec actor.
%% self() ->
%%     % check if in the exec actor

%%%=========================================================================
%%%  Internal Function
%%%=========================================================================

exec(Arg) ->
    %% erlang:display("exec receiveed msg"),
    case Arg of
	{apply, F, V, From} ->
	    apply(F, [V, From]),
	    From ! 'end',
	    core:become(fun exec/1);
	{apply, F, V, From, N} ->
	    apply(F, [V]),
	    From ! {'end', N},
	    core:become(fun exec/1)
    end.

meta1(Q, F, S) ->
    fun (M) ->
	    %% erlang:display("meta1 receiveed following msg"),
	    %% erlang:display(M),
	    case M of
		{mesg, V} ->
		    %% erlang:display("meta1 matched mesg clause"),
		    case S of
			dormant ->
			    self() ! 'begin',
			    core:become(meta1(Q++[V], F, active));
			active ->
			    core:become(meta1(Q++[V], F, active))
		    end;
		'begin' ->
		    %% erlang:display("meta1 matched begin clause"),
		    %% erlang:display(Q),
		    case Q of
			[V|_Q] ->
			    exec ! {apply, F, V, self()},
			    core:become(meta1(_Q, F, S))
		    end;
		'end' ->
		    case Q of
			[] -> core:become(meta1(Q, F, dormant));
			[_|_] ->
			    self() ! 'begin',
			    core:become(meta1(Q, F, S))
		    end
	    end
    end.

metaG(Qs, Fs, Ss) ->
    fun (M) ->
	    case M of
		{mesg, {N, V}} ->
		    NthSs = nth(N, Ss),
		    case NthSs of
			dormat ->
			    self() ! {'begin', N},
			    core:become(metaG(substNth(N, nth(N,Qs)++[V], Qs), Fs, substNth(N, active, Ss)));
			active ->
			    core:become(metaG(substNth(N, nth(N,Qs)++[V], Qs), Fs, substNth(N, active, Ss)))
		    end;
		{'begin', N} ->
		    case nth(N, Qs) of
			[V|_Q] ->
			    exec ! {apply, nth(N, Fs), V, self(), N},
			    core:become(metaG(substNth(N, _Q, Qs), Fs, Ss))
		    end;
		{'end', N} ->
		    case nth(N, Qs) of
			[] -> core:become(meta1(Qs, Fs, substNth(N, dormant, Ss)));
			[_|_] ->
			    self() ! {'begin', N},
			    core:become(meta1(Qs, Fs, Ss))
		    end
	    end
    end.

%% ----------- Utils -----------

nth(N, [H|T]) ->
    case N of
	1 -> H;
	N when N > 1 -> nth(N-1, T)
    end.

substNth(N, V, Ls) ->
    case {Ls, N} of
	{[_|T], 1} -> [V|T];
	{[H|T], N} when N > 1 -> [H|substNth(N-1, V, T)]
    end.
