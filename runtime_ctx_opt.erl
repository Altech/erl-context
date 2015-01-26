-module(runtime_ctx_opt).
-export([new_group/1, send/2, new/1, send_context/2, send_delay/3, send_context_delay/3]).

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

new_group(Fs) ->
    N = length(Fs),
    core:new(meta_group(replicate(N, []), Fs, replicate(N, dormant), replicate(N, context:default()), replicate(N, log:new()), core:new(fun exec/1))).

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

send_context(Dest, Context) ->    
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
send_delay(Dest, Msg, Delay) ->    
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

send_context_delay(Dest, Context, Delay) ->    
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
	    core:become(fun exec1);
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

meta_group(Qs, Fs, Ss, Cs, Ls, E) ->
    fun (RawM) ->
            %% io:format("meta_group: received ~p~n", [RawM]),
            case RawM of
                {mesg, {N, M}, Ext} ->
                    case nth(N, Ss) of
                        dormant ->
                            self() ! {'begin', N, []},
                            core:become(meta_group(subst_nth(N, nth(N,Qs)++[{M, Ext}], Qs), Fs, subst_nth(N, active, Ss), Cs, Ls, E));
                        active ->
                            core:become(meta_group(subst_nth(N, nth(N,Qs)++[{M, Ext}], Qs), Fs, subst_nth(N, active, Ss), Cs, Ls, E))
                    end;
                {'begin', N, _} ->
                    [[{M, Ext}|_Q], F, C, L] = [nth(N, Qs), nth(N, Fs), nth(N, Cs), nth(N, Ls)],
                    case proplists:get_value(context, Ext) of 
                        message ->
                            self() ! {'end', N, [{sent_messages, []}]},
                            case context:compare(C, M) of
                                newer ->
                                    NewLs = subst_nth(N, log:log_before(L, proplists:get_value(id, Ext), {M, Ext}, C, F), Ls),
                                    core:become(meta_group(subst_nth(N, _Q, Qs), Fs, Ss, subst_nth(N, M, Cs), NewLs, E));
                                _ -> 
                                    core:become(meta_group(subst_nth(N, _Q, Qs), Fs, Ss, Cs, Ls, E))
                            end;
                        {'$context', _} = WithC ->
                            case context:compare(C, WithC) of
                                newer ->
                                    E ! {apply, F, M, self(), WithC, N},
                                    NewLs = subst_nth(N, log:log_before(L, proplists:get_value(id, Ext), {M, Ext}, C, F), Ls),
                                    core:become(meta_group(subst_nth(N, _Q, Qs), Fs, Ss, subst_nth(N, WithC, Cs), NewLs, E));
                                older ->
                                    MesgToCancel = element_after(fun(E) -> context:compare(WithC, log:before_context(E)) == newer end, L),
                                    [BackedQs, BackedFs, BackedCs, BackedLs] = cancel_messages_after({N, MesgToCancel}, subst_nth(N, _Q, Qs), Fs, Cs, Ls),
                                    E ! {apply, F, M, self(), WithC, N},
                                    NewBackedLs = subst_nth(N, log:log_before(nth(N, BackedLs), proplists:get_value(id, Ext), {M, Ext}, WithC, nth(N, BackedFs)), BackedLs),
                                    core:become(meta_group(BackedQs, BackedFs, Ss, subst_nth(N, WithC, BackedCs), NewBackedLs, E));
                                same ->
                                    E ! {apply, F, M, self(), C, N},
                                    NewLs = subst_nth(N, log:log_before(L, proplists:get_value(id, Ext), {M, Ext}, C, F), Ls),
                                    core:become(meta_group(subst_nth(N, _Q, Qs), Fs, Ss, Cs, NewLs, E))
                            end;
                        undefined ->
                            NewLs = subst_nth(N, log:log_before(L, proplists:get_value(id, Ext), {M, Ext}, C, F), Ls),
                            E ! {apply, F, M, self(), C, N},
                            core:become(meta_group(subst_nth(N, _Q, Qs), Fs, Ss, Cs, NewLs, E))
                    end;
                {'end', N, Ext} ->
                    NewLs = subst_nth(N, log:log_after(nth(N,Ls), nth(N,Cs), nth(N,Fs), proplists:get_value(sent_messages, Ext)), Ls),
                    case nth(N, Qs) of
                        [] -> core:become(meta_group(Qs, Fs, subst_nth(N, dormant, Ss), Cs, NewLs, E));
                        [_|_] ->
                            self() ! {'begin', N, []},
                            core:become(meta_group(Qs, Fs, Ss, Cs, NewLs, E))
                    end;
                {new, F, From, Ext} ->
                    N = length(Qs) + 1,
                    From ! {N, self()},
                    core:become(meta_group(Qs++[[]], Fs++[F], Ss++[dormant], Cs++[proplists:get_value(context, Ext)], Ls++[log:new()], E));
                inspect -> % for debug
                    erlang:display([Qs, Fs, Ss, Cs, Ls]),
                    core:become(meta_group(Qs, Fs, Ss, Cs, Ls, E));
                {getState, From} -> % for debug
                    From ! {Qs, Fs, Ss, Cs, Ls},
                    core:become(meta_group(Qs, Fs, Ss, Cs, Ls, E))
            end
    end.

cancel_messages_after({N, E}, Qs, Fs, Cs, Ls) ->
    [MesgsToCancel, MesgsToQueue] = collect_messages_to_cancel([{N, E}], ordsets:new(), ordsets:new(), Ls),
    [LsToCancel, LsToQueue] = [make_partial_log_list(List, Ls) || List <- [MesgsToCancel, MesgsToQueue]],
    [
     [lists:map(fun(_E)-> log:message(_E) end, L) ++ Q       || {Q, L} <- lists:zip(Qs, LsToQueue)],
     [case L of [H|T] -> log:before_function(H); [] -> F end || {F, L} <- lists:zip(Fs, LsToCancel)],
     [case L of [H|T] -> log:before_context(H);  [] -> C end || {C, L} <- lists:zip(Cs, LsToCancel)],
     make_removed_log_list(MesgsToCancel, Ls)
    ].

collect_messages_to_cancel([], Checked, Derived, Ls) ->
    [Checked, ordsets:subtract(Checked, Derived)];
collect_messages_to_cancel([{N,E}|UnChecked], Checked, Derived, Ls) ->
    MesgsAfterE = [{N, _E} || _E <- lists:takewhile(fun(_E) -> log:message_ID(_E) /= log:message_ID(E) end, nth(N, Ls))],
    SentMesgsOfE = [{_N, log:lookup(_ID, nth(_N, Ls))} || {_N, _ID} <- log:sent_message_ID_and_dest_numbers(E)],
    collect_messages_to_cancel([{_N, _E} || {_N, _E} <- MesgsAfterE ++ SentMesgsOfE, not ordsets:is_element({N, log:message_ID(_E)}, Checked)] ++ UnChecked, 
                               ordsets:add_element({N, log:message_ID(E)}, Checked), 
                               add_elements([{_N, log:message_ID(_E)} || {_N, _E} <- SentMesgsOfE], Derived),
                               Ls).

make_partial_log_list(MesgList, Ls) ->
    lists:map(fun(N) -> 
                      MesgsToCancelOfN = [tuple_reverse(log:lookup_with_index(_ID, nth(_N, Ls))) || {_N, _ID} <- MesgList, _N == N],
                      [_E || {_I, _E} <- lists:reverse(lists:sort(MesgsToCancelOfN))]
              end, lists:seq(1, length(Ls))).

make_removed_log_list([], Ls) ->
    Ls;
make_removed_log_list([{N, ID}|MesgList], Ls) ->
    make_removed_log_list(MesgList,
                          subst_nth(N,[_E || _E <- nth(N, Ls), log:message_ID(_E) /= ID],Ls)).

%% ----------- Utils -----------

nth(N, [H|T]) ->
    case N of
        1 -> H;
        N when N > 1 -> nth(N-1, T)
    end.

subst_nth(N, V, Ls) ->
    case {Ls, N} of
        {[_|T], 1} -> [V|T];
        {[H|T], N} when N > 1 -> [H|subst_nth(N-1, V, T)]
    end.

replicate(N, V) ->
    case N of
        0 -> [];
        N when N > 0 -> [V|replicate(N-1, V)]
    end.

element_after(Pred, Ls) ->
    hd(lists:dropwhile(Pred, Ls)).

gen_ID() -> base64:encode(crypto:strong_rand_bytes(4)).

tuple_reverse({E1, E2}) ->
    {E2, E1}.

add_elements(Ls, Set) ->
    lists:foldl(fun(E, Set)-> ordsets:add_element(E, Set) end, Set, Ls).
