-module(runtime_ctx_opt).
-export([newG/1, send/2, new/1, sendContext/2, sendDelay/3, sendContextDelay/3]).

%%%=========================================================================
%%%  API
%%%=========================================================================

new(F) ->
    case get(self) of
	undefined -> 
	    core:new(runtime:meta1([], F, dormant, core:new(fun exec/1)));
	{_, MetaG} ->
	    MetaG ! {new, F, self(), [{context, get(context)}]},
	    core:become(fun(X) -> X end)
    end.

newG(Fs) ->
    N = length(Fs),
    core:new(metaCtx(replicate(N, []), Fs, replicate(N, dormant), replicate(N, context:default()), replicate(N, log:new()), core:new(fun exec/1))).

send(Dest, Msg) ->    
    case Dest of 
	{N, _Dest} -> case get(context) of
			  undefined -> _Dest ! {mesg, {N, Msg}, []};
			  _ ->         ID = gen_ID(),
			               put(sent_messages, [{Dest, {ID, Msg}}| get(sent_messages)]),
				       _Dest ! {mesg, {N, Msg}, [{id, ID}, {context, get(context)}]}
		      end;
	_ -> Dest ! {mesg, Msg, []}
    end.

sendContext(Dest, Context) ->    
    case Dest of 
	{N, _Dest} -> case get(context) of
			  undefined -> _Dest ! {mesg, {N, Context}, [{context, message}]};
			  _ ->         ID = gen_ID(),
				       put(sent_messages, [{Dest, {ID, Context}}| get(sent_messages)]),
                                       _Dest ! {mesg, {N, Context}, [{id, ID}, {context, message}]}
		      end;
	_ -> Dest ! {mesg, Context, []}
    end.

% For Experiments
sendDelay(Dest, Msg, Delay) ->    
    PContext = get(context),
    ID = gen_ID(),
    spawn(fun() -> 
		  timer:sleep(Delay),
		  case Dest of 
		      {N, _Dest} -> case PContext of
					undefined -> _Dest ! {mesg, {N, Msg}, []};
					_ ->         _Dest ! {mesg, {N, Msg}, [{id, ID}, {context, PContext}]}
				    end;
		      _ -> Dest ! {mesg, Msg, []}
		  end
	  end),
    case {Dest, get(context)} of 
	{{N, _Dest}, {'$context', _}} -> put(sent_messages, [{Dest, {ID, Msg}}| get(sent_messages)]); _ -> nil
    end.

sendContextDelay(Dest, Context, Delay) ->    
    PContext = get(context),
    ID = gen_ID(),
    spawn(fun() -> 
		  timer:sleep(Delay),
		  case Dest of 
		      {N, _Dest} -> case PContext of
					undefined -> _Dest ! {mesg, {N, Context}, [{context, message}]};
					_ ->         _Dest ! {mesg, {N, Context}, [{id, ID}, {context, message}]}
				    end;
		      _ -> Dest ! {mesg, Context, []}
		  end
	  end),
    case {Dest, get(context)} of 
	{{N, _Dest}, {'$context', _}} -> put(sent_messages, [{Dest, {ID, Context}}| get(sent_messages)]); _ -> nil
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
	    From ! {'end', N, []},
	    core:become(fun exec/1);
	% From Group-Wide Context-Aware Meta-Level
	{apply, F, M, From, Ctx, N} ->
	    put(self, {N, From}),
	    put(context, Ctx),
	    put(sent_messages, []),
	    apply(F, [M]),
	    From ! {'end', N, [{sent_messages, get(sent_messages)}]},
	    core:become(fun exec/1)
    end.

metaCtx(Qs, Fs, Ss, Cs, Ls, E) ->
    fun (RawM) ->
	    io:format("metaCtx: received ~p~n", [RawM]),
	    case RawM of
		{mesg, {N, M}, Ext} ->
		    case nth(N, Ss) of
			dormant ->
			    self() ! {'begin', N, []},
			    core:become(metaCtx(substNth(N, nth(N,Qs)++[{M, Ext}], Qs), Fs, substNth(N, active, Ss), Cs, Ls, E));
			active ->
			    core:become(metaCtx(substNth(N, nth(N,Qs)++[{M, Ext}], Qs), Fs, substNth(N, active, Ss), Cs, Ls, E))
		    end;
		{'begin', N, _} ->
		    [[{M, Ext}|_Q], F, C, L] = [nth(N, Qs), nth(N, Fs), nth(N, Cs), nth(N, Ls)],
		    case proplists:get_value(context, Ext) of 
			message ->
			    self() ! {'end', N, [{sent_messages, []}]},
			    case (context:compare(C, M) == newer) and lists:all(fun({_, _C}) -> context:compare(C, _C) == older end, _Q) of 
			    	true  -> 
				    NewLs = substNth(N, log:logBefore(L, proplists:get_value(id, Ext), M, C, F), Ls),
				    core:become(metaCtx(substNth(N, _Q, Qs), Fs, Ss, substNth(N, M, Cs), NewLs, E));
			    	false -> 
				    core:become(metaCtx(substNth(N, _Q++[M], Qs), Fs, Ss, Cs, Ls, E))
			    end;
			{'$context', _} = WithC ->
			    NewLs = substNth(N, log:logBefore(L, proplists:get_value(id, Ext), M, C, F), Ls),
			    case context:compare(C, WithC) of
			    	newer ->
			    	    E ! {apply, F, M, self(), WithC, N},
			    	    core:become(metaCtx(substNth(N, _Q, Qs), Fs, Ss, substNth(N, WithC, Cs), NewLs, E));
			    	_ ->
			    	    E ! {apply, F, M, self(), C, N},
			    	    core:become(metaCtx(substNth(N, _Q, Qs), Fs, Ss, Cs, NewLs, E))
			    end;
			undefined ->
			    NewLs = substNth(N, log:logBefore(L, proplists:get_value(id, Ext), M, C, F), Ls),
			    E ! {apply, F, M, self(), C, N},
			    core:become(metaCtx(substNth(N, _Q, Qs), Fs, Ss, Cs, NewLs, E))
		    end;
		% Original plus Messages list
		{'end', N, Ext} ->
		    NewLs = substNth(N, log:logAfter(nth(N,Ls), nth(N,Cs), nth(N,Fs), proplists:get_value(sent_messages, Ext)), Ls),
		    case nth(N, Qs) of
			[] -> core:become(metaCtx(Qs, Fs, substNth(N, dormant, Ss), Cs, NewLs, E));
			[_|_] ->
			    self() ! {'begin', N, []},
			    core:become(metaCtx(Qs, Fs, Ss, Cs, NewLs, E))
		    end;
		{new, F, From, Ext} ->
		    N = length(Qs) + 1,
		    From ! {N, self()},
		    core:become(metaCtx(Qs++[[]], Fs++[F], Ss++[dormant], Cs++[proplists:get_value(context, Ext)], Ls++[log:new()], E));
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

gen_ID() -> base64:encode(crypto:strong_rand_bytes(4)).
