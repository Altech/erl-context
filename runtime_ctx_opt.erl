-module(runtime_ctx_opt).
-export([newG/1, send/2, new/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================

new(F) ->
    case get(self) of
	undefined -> 
	    core:new(runtime:meta1([], F, dormant, core:new(fun exec/1)));
	{_, MetaG} ->
	    MetaG ! {new, F, self(), get(context)},
	    core:become(fun(X) -> X end)
    end.

newG(Fs) ->
    N = length(Fs),
    core:new(metaCtx(replicate(N, []), Fs, replicate(N, dormant), replicate(N, context:default()), replicate(N, log:new()), core:new(fun exec/1))).

send(Dest, Msg) ->    
    case Dest of 
	{N, _Dest} -> case get(context) of
			  undefined -> _Dest ! {mesg, {N, Msg}}; 
			  _ ->         _Dest ! {mesg, {N, Msg, get(context)}}
		      end;
	_ -> Dest ! {mesg, Msg}
    end.

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
	    From ! {'end', N},
	    core:become(fun exec/1);
	% From Group-Wide Context-Aware Meta-Level
	{apply, F, M, From, Ctx, N} ->
	    put(self, {N, From}),
	    put(context, Ctx),
	    apply(F, [M]),
	    From ! {'end', N},
	    core:become(fun exec/1)
    end.

metaCtx(Qs, Fs, Ss, Cs, Ls, E) ->
    fun (RawM) ->
	    io:format("metaCtx: received ~p~n", [RawM]),
	    case RawM of
		{mesg, {N, M}} ->
		    case nth(N, Ss) of
			dormant ->
			    self() ! {'begin', N},
			    core:become(metaCtx(substNth(N, nth(N,Qs)++[M], Qs), Fs, substNth(N, active, Ss), Cs, Ls, E));
			active ->
			    core:become(metaCtx(substNth(N, nth(N,Qs)++[M], Qs), Fs, substNth(N, active, Ss), Cs, Ls, E))
		    end;
		% Extension
		{mesg, {N, M, {'$context', _} = C}} ->
		    case nth(N, Ss) of
			dormant ->
			    self() ! {'begin', N},
			    core:become(metaCtx(substNth(N, nth(N,Qs)++[{M, C}], Qs), Fs, substNth(N, active, Ss), Cs, Ls, E));
			active ->
			    core:become(metaCtx(substNth(N, nth(N,Qs)++[{M, C}], Qs), Fs, substNth(N, active, Ss), Cs, Ls, E))
		    end;
		{'begin', N} ->
		    [Q, F, C, L] = [nth(N, Qs), nth(N, Fs), nth(N, Cs), nth(N, Ls)],
		    case Q of
			% Extension
			[{M, {'$context', _} = WithC}|_Q] ->
			    case context:compare(C, WithC) of
			    	newer ->
				    NewLs = substNth(N, log:logBefore(L, M, WithC, F), Ls),
			    	    E ! {apply, F, M, self(), WithC, N},
			    	    core:become(metaCtx(substNth(N, _Q, Qs), Fs, Ss, substNth(N, WithC, Cs), NewLs, E));
			    	_ ->
				    NewLs = substNth(N, log:logBefore(L, M, C, F), Ls),
			    	    E ! {apply, F, M, self(), C, N},
			    	    core:become(metaCtx(substNth(N, _Q, Qs), Fs, Ss, Cs, NewLs, E))
			    end;
			%% % Extension
			%% [{'$context', _} = NewC|_Q] ->
			%%     self() ! {'end', N},
			%%     case (context:compare(C, NewC) == newer) and lists:all(fun({_, _C}) -> context:compare(C, _C) == older end, _Q) of 
			%%     	true  -> 
			%% 	    core:become(metaCtx(substNth(N, _Q, Qs), Fs, Ss, substNth(N, NewC, Cs), substNth(N, log:logBefore(L, NewC, C, F), Ls), E));
			%%     	false -> 
			%% 	    core:become(metaCtx(substNth(N, _Q++[NewC], Qs), Fs, Ss, Cs, Ls, E))
			%%     end;
			% Original plus logging
			[M|_Q] ->
			    NewLs = substNth(N, log:logBefore(L, M, C, F), Ls),
			    E ! {apply, F, M, self(), C, N},
			    core:become(metaCtx(substNth(N, _Q, Qs), Fs, Ss, Cs, NewLs, E))
		    end;
		{'end', N} ->
		    io:format("Log: ~p~n", [nth(N,Ls)]),
		    NewLs = substNth(N, log:logAfter(nth(N,Ls), nth(N,Cs), nth(N,Fs)), Ls),
		    case nth(N, Qs) of
			[] -> core:become(metaCtx(Qs, Fs, substNth(N, dormant, Ss), Cs, NewLs, E));
			[_|_] ->
			    self() ! {'begin', N},
			    core:become(metaCtx(Qs, Fs, Ss, Cs, NewLs, E))
		    end;
		{new, F, From, C} ->
		    N = length(Qs) + 1,
		    From ! {N, self()},
		    core:become(metaCtx(Qs++[[]], Fs++[F], Ss++[dormant], Cs++[C], Ls++[log:new()], E));
		inspect -> % for debug
		    erlang:display({Qs, Fs, Ss, Cs, Ls}),
		    core:become(metaCtx(Qs, Fs, Ss, Cs, Ls, E));
		{getState, From} -> % for debug
		    From ! {Qs, Fs, Ss, Cs, Ls},
		    core:become(metaCtx(Qs, Fs, Ss, Cs, Ls, E))
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
