-module(runtime).
-export([new/1, send/2, newG/1, usr_self/0, neighbor/1, change_behavior/2]).

%%%=========================================================================
%%%  API
%%%=========================================================================

new(F) ->
    case get(self) of
	undefined -> 
	    core:new(meta1([], F, dormant, core:new(fun exec/1)));
	{_, MetaG} ->
	    MetaG ! {new, F, self(), []},
	    core:become(fun(X) -> X end)
    end.

newG(Fs) ->
    N = length(Fs),
    core:new(metaG(replicate(N, []), Fs, replicate(N, dormant), core:new(fun exec/1))).

%% send V to {N1, ... {Nn, M}} is 
send(Dest, Msg) ->
    case Dest of 
	{N, _Dest} -> _Dest ! {mesg, {N, Msg}, []};
	_ -> Dest ! {mesg, Msg, []}
    end.

usr_self() -> 
    case get(self) of
	undefined -> self();
	Self -> Self
    end.

neighbor(N) ->
    case get(self) of
	{_, MetaG} -> {N, MetaG}
    end.

change_behavior(F, {N, MetaG}) ->
    MetaG ! {update, N, F}.

%%%=========================================================================
%%%  Internal Function
%%%=========================================================================

exec(Arg) ->
    %% io:format("engine received ~p.~n",[Arg]),
    case Arg of
	% From Per-Actor Meta-Level
	{apply, F, M, From} ->
	    put(self, From),
	    apply(F, [M]),
	    From ! 'end',
	    core:become(fun exec/1);
	% From Group-Wide Meta-Level
	{apply, F, M, From, N} ->
	    put(self, {N, From}),
	    apply(F, [M]),
	    From ! {'end', N, []},
	    core:become(fun exec/1)    
    end.

meta1(Q, F, S, E) ->
    fun (RawM) ->
	    case RawM of
		{mesg, M} ->
		    case S of
			dormant ->
			    self() ! 'begin',
			    core:become(meta1(Q++[M], F, active, E));
			active ->
			    core:become(meta1(Q++[M], F, active, E))
		    end;
		'begin' ->
		    case Q of
			[M|_Q] ->
			    E ! {apply, F, M, self()},
			    core:become(meta1(_Q, F, S, E))
		    end;
		'end' ->
		    case Q of
			[] -> core:become(meta1(Q, F, dormant, E));
			[_|_] ->
			    self() ! 'begin',
			    core:become(meta1(Q, F, S, E))
		    end
	    end
    end.

metaG(Qs, Fs, Ss, E) ->
    fun (RawM) ->
	    case RawM of
		{mesg, {N, M}, _} ->
		    case nth(N, Ss) of
			dormant ->
			    self() ! {'begin', N, []},
			    core:become(metaG(substNth(N, nth(N,Qs)++[M], Qs), Fs, substNth(N, active, Ss), E));
			active ->
			    core:become(metaG(substNth(N, nth(N,Qs)++[M], Qs), Fs, substNth(N, active, Ss), E))
		    end;
		{'begin', N, _} ->
		    case nth(N, Qs) of
			[M|_Q] ->
			    E ! {apply, nth(N, Fs), M, self(), N},
			    core:become(metaG(substNth(N, _Q, Qs), Fs, Ss, E))
		    end;
		{'end', N, _} ->
		    case nth(N, Qs) of
			[] -> core:become(metaG(Qs, Fs, substNth(N, dormant, Ss), E));
			[_|_] ->
			    self() ! {'begin', N, []},
			    core:become(metaG(Qs, Fs, Ss, E))
		    end;
		{new, F, From, _} ->
		    N = length(Qs) + 1,
		    From ! {N, self()},
		    core:become(metaG(Qs++[[]], Fs++[F], Ss++[dormant], E));
		{change_behavior, N, F} ->
		    core:become(metaG(Qs, substNth(N, F, Fs), Ss, E));
		inspect -> % for debug
		    erlang:display({Qs, Fs, Ss}),
		    core:become(metaG(Qs, Fs, Ss, E))
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
