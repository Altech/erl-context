-module(runtime).
-export([start/0, restart/0, exec/1, new/1, send/2, newG/1, new/2]).

%%%=========================================================================
%%%  API
%%%=========================================================================

start() ->
    % register, whereis はグローバル変数みたいなもの。これを使ってしまっている。
    MaybePid = whereis(exec),
    if MaybePid == undefined -> register(exec, core:new(fun exec/1));
       true                  -> {already_started, MaybePid}
    end.

restart() ->
    MaybePid = whereis(exec),
    if MaybePid /= undefined -> exit(MaybePid, kill); true -> true end,
    catch unregister(exec), 
    Pid = core:new(fun exec/1),
    register(exec, Pid),
    Pid.

new(F) ->
    core:new(meta1([], F, dormant)).

%% send V to {N1, ... {Nn, M}} is 
send(Dest, Msg) ->
    case Dest of 
	{N, _Dest} -> _Dest ! {mesg, {N, Msg}};
	_ -> Dest ! {mesg, Msg}
    end.

%% %% This will be called in exec actor.
%% self() ->
%%     % check if in the exec actor

newG(Fs) ->
    N = length(Fs),
    core:new(metaG(replicate(N, []), Fs, replicate(N, dormant))).

new({N, MetaG}, F) ->
    MetaG ! {new, F, self()},
    % becomeの返り値を利用してしまっている
    core:become(fun(X) -> X end).

%%%=========================================================================
%%%  Internal Function
%%%=========================================================================

exec(Arg) ->
    case Arg of
	{apply, F, V, From} ->
	    apply(F, [V, From]),
	    From ! 'end',
	    core:become(fun exec/1);
	{apply, F, V, From, N} ->
	    apply(F, [V, {N, From}]),
	    From ! {'end', N},
	    core:become(fun exec/1)
    end.

meta1(Q, F, S) ->
    fun (M) ->
	    case M of
		{mesg, V} ->
		    case S of
			dormant ->
			    self() ! 'begin',
			    core:become(meta1(Q++[V], F, active));
			active ->
			    core:become(meta1(Q++[V], F, active))
		    end;
		'begin' ->
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
			dormant ->
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
			[] -> core:become(metaG(Qs, Fs, substNth(N, dormant, Ss)));
			[_|_] ->
			    self() ! {'begin', N},
			    core:become(metaG(Qs, Fs, Ss))
		    end;
		{new, F, From} ->
		    N = length(Qs) + 1,
		    From ! {N, self()},
		    core:become(metaG(Qs++[[]], Fs++[F], Ss++[dormant]));
		inspect -> % for debug
		    erlang:display({Qs, Fs, Ss}),
		    core:become(metaG(Qs, Fs, Ss))
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

replicate(N, V) ->
    case N of
	0 -> [];
	N when N > 0 -> [V|replicate(N-1, V)]
    end.
