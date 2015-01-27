-module(runtime_gwrc).
-export([new/1, send/2, new_group/1, send_delay/3, send_context/2, send_context_delay/3, set_context/1, self_context/0]).
-import(general,[nth/2, subst_nth/3, replicate/2, element_after/2, tuple_reverse/1, remove_duplicate/1, add_elements/2, l/1, l/2]).
-export([id_server_for_debug/1]).

%%%=========================================================================
%%%  API
%%%=========================================================================

new(F) ->
    case get(self) of
	undefined -> 
	    runtime_base:new(runtime:meta1([], F, dormant, runtime_base:new(fun exec/1)));
	{_, MetaG} ->
	    MetaG ! {new, F, self(), [{context, get(context)}]},
	    runtime_base:become(fun(X) -> X end)  % 返り値を使用（注：モデルからの逸脱）
    end.

new_group(Fs) ->
    N = length(Fs),
    Es = lists:map(fun(_)-> runtime_base:new(fun exec/1) end, lists:seq(1,N)),
    runtime_base:new(meta_group(replicate(N, []), Fs, replicate(N, dormant), replicate(N, context:default()), replicate(N, log:new()), Es, {0, []})).

send(Dest, Msg) ->    
    case Dest of 
	{N, _Dest} -> case get(context) of
			  undefined -> _Dest ! {mesg, {N, Msg}, []};
			  _ ->         ID = gen_ID(),
                                       l("(~p) node~p -> node~p ~p",[self_context(), element(1,get(self))-1, N-1,Msg]),
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

set_context(ContextValue) ->
    case {get(self), get(context)} of
        {{N, MetaG}, {'$context', _}} ->
            C = context:new(ContextValue),
            put(context, C),
            MetaG ! {set_context, N, C, runtime_base:self(), []},
            runtime_base:become(fun(end_context) -> ok end);
        _ -> failure
    end.

self_context() ->
    C = get(context),
    case C of
        {'$context', _} -> context:value(C);
        _ -> none
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
	    runtime_base:become(fun exec/1);
        % From Group-Wide Meta-Level
	{apply, F, M, From, N} ->
	    put(self, {N, From}),
            apply(F, [M]),
            From ! {'end', N, []},
            runtime_base:become(fun exec/1);
        % From Group-Wide Context-Aware Meta-Level
        {apply, F, M, From, Ctx, N} ->
            put(self, {N, From}),
            put(context, Ctx),
            put(sent_messages, []),
            apply(F, [M]),
            From ! {'end', N, [{sent_messages, get(sent_messages)}]},
            runtime_base:become(fun exec/1)
    end.

meta_group(Qs, Fs, Ss, Cs, Ls, Es, Rb) ->
    fun (RawM) ->
            %% io:fwrite("RawM:~p Rb:~p~n",[RawM, Rb]),
            if 
                length(element(2,Rb)) > 0 ->
                    meta_group_wait(RawM, Qs, Fs, Ss, Cs, Ls, Es, Rb);
                true ->
                    case RawM of
                        {mesg, {N, M}, Ext} ->
                            case nth(N, Ss) of
                                dormant ->
                                    self() ! {'begin', N, []},
                                    runtime_base:become(meta_group(subst_nth(N, nth(N,Qs)++[{M, Ext}], Qs), Fs, subst_nth(N, active, Ss), Cs, Ls, Es, Rb));
                                active ->
                                    runtime_base:become(meta_group(subst_nth(N, nth(N,Qs)++[{M, Ext}], Qs), Fs, subst_nth(N, active, Ss), Cs, Ls, Es, Rb))
                            end;
                        {'begin', N, _} ->
                            meta_group_begin(RawM, Qs, Fs, Ss, Cs, Ls, Es, Rb);
                        {'end', N, Ext} ->
                            NewLs = subst_nth(N, log:log_after(nth(N,Ls), nth(N,Cs), nth(N,Fs), proplists:get_value(sent_messages, Ext)), Ls),
                            case nth(N, Qs) of
                                [] -> runtime_base:become(meta_group(Qs, Fs, subst_nth(N, dormant, Ss), Cs, NewLs, Es, Rb));
                                [_|_] ->
                                    self() ! {'begin', N, []},
                                    runtime_base:become(meta_group(Qs, Fs, Ss, Cs, NewLs, Es, Rb))
                            end;
                        {new, F, From, Ext} ->
                            N = length(Qs) + 1,
                            From ! {N, self()}, % [FIXME]
                            runtime_base:become(meta_group(Qs++[[]], Fs++[F], Ss++[dormant], Cs++[proplists:get_value(context, Ext)], Ls++[log:new()], Es, Rb));
                        {become, N, F, From, _} ->
                            From ! end_become,
                            runtime_base:become(meta_group(Qs, subst_nth(N, F, Fs), Ss, Cs, Ls, Es, Rb));
                        {set_context, N, C, From, _} ->
                            From ! end_context,
                            runtime_base:become(meta_group(Qs, Fs, Ss, subst_nth(N, C, Cs), Ls, Es, Rb));
                        inspect -> % for debug
                            erlang:display([Qs, Fs, Ss, Cs, Ls]),
                            runtime_base:become(meta_group(Qs, Fs, Ss, Cs, Ls, Es, Rb));
                        {getState, From} -> % for debug
                            From ! {Qs, Fs, Ss, Cs, Ls},
                            runtime_base:become(meta_group(Qs, Fs, Ss, Cs, Ls, Es, Rb))
                    end
            end
    end.

meta_group_begin(RawM, Qs, Fs, Ss, Cs, Ls, Es, Rb) ->
    {'begin', N, _} = RawM,
    [[{M, Ext}|_Q], F, C, L, E] = [nth(N, Qs), nth(N, Fs), nth(N, Cs), nth(N, Ls), nth(N, Es)],
    case proplists:get_value(context, Ext) of 
        message ->
            self() ! {'end', N, [{sent_messages, []}]},
            case context:compare(C, M) of
                newer -> % [TODO] 外から来てIDが無い場合
                    NewLs = subst_nth(N, log:log_before(L, proplists:get_value(id, Ext), {M, Ext}, C, F), Ls),
                    runtime_base:become(meta_group(subst_nth(N, _Q, Qs), Fs, Ss, subst_nth(N, M, Cs), NewLs, Es, Rb));
                _ -> 
                    runtime_base:become(meta_group(subst_nth(N, _Q, Qs), Fs, Ss, Cs, Ls, Es, Rb))
            end;
        {'$context', _} = WithC ->
            case context:compare(C, WithC) of
                newer ->
                    E ! {apply, F, M, self(), WithC, N},
                    NewLs = subst_nth(N, log:log_before(L, proplists:get_value(id, Ext), {M, Ext}, C, F), Ls),
                    runtime_base:become(meta_group(subst_nth(N, _Q, Qs), Fs, Ss, subst_nth(N, WithC, Cs), NewLs, Es, Rb));
                older ->
                    io:fwrite("<rollback> by ~p(~p) to node~p~n",[M,proplists:get_value(id, Ext),N-1]),
                    MesgToCancel = element_after(fun(Elm) -> context:compare(WithC, log:before_context(Elm)) == newer end, L),
                    %% io:fwrite(" cancel_messages_after(~p|~p)~n", [log:message_ID(MesgToCancel), element(1, log:message(MesgToCancel))]),
                    V = cancel_messages_after({N, MesgToCancel}, subst_nth(N, _Q, Qs), Fs, Cs, Ls),
                    case V of 
                        {waiting, Ws} -> 
                            io:fwrite(" ...waiting ~p~n",[Ws]),
                            runtime_base:become(meta_group(Qs, Fs, Ss, Cs, Ls, Es, {N, Ws}));
                        [BackedQs, BackedFs, BackedCs, BackedLs] ->
                            %% io:fwrite(" BackedQs[~p]=~p~n",[N, nth(N,BackedQs)]),
                            E ! {apply, F, M, self(), WithC, N},
                            NewBackedLs = subst_nth(N, log:log_before(nth(N, BackedLs), proplists:get_value(id, Ext), {M, Ext}, WithC, nth(N, BackedFs)), BackedLs),
                            runtime_base:become(meta_group(BackedQs, BackedFs, Ss, subst_nth(N, WithC, BackedCs), NewBackedLs, Es, Rb))
                    end;
                same ->
                    E ! {apply, F, M, self(), C, N},
                    NewLs = subst_nth(N, log:log_before(L, proplists:get_value(id, Ext), {M, Ext}, C, F), Ls),
                    runtime_base:become(meta_group(subst_nth(N, _Q, Qs), Fs, Ss, Cs, NewLs, Es, Rb))
            end;
        undefined ->
            NewLs = subst_nth(N, log:log_before(L, proplists:get_value(id, Ext), {M, Ext}, C, F), Ls),
            E ! {apply, F, M, self(), C, N},
            runtime_base:become(meta_group(subst_nth(N, _Q, Qs), Fs, Ss, Cs, NewLs, Es, Rb))
    end.

meta_group_wait(RawM, Qs, Fs, Ss, Cs, Ls, Es, Rb) ->
    case RawM of
        {'end', N, Ext} ->
            {M, Ws} = Rb,
            NewLs = subst_nth(N, log:log_after(nth(N,Ls), nth(N,Cs), nth(N,Fs), proplists:get_value(sent_messages, Ext)), Ls),
            NewWs = lists:filter(fun (_N) -> N /= _N end, Ws),
            if length(NewWs) == 0 -> 
                    case nth(N, Qs) of
                        [] ->  meta_group_begin({'begin', M, []}, Qs, Fs, subst_nth(N, dormant, Ss), Cs, NewLs, Es, {0, []});
                        [_|_] -> meta_group_begin({'begin', M, []}, Qs, Fs, Ss, Cs, NewLs, Es, {0, []})
                    end;
               true ->
                    case nth(N, Qs) of
                        [] -> runtime_base:become(meta_group(Qs, Fs, subst_nth(N, dormant, Ss), Cs, NewLs, Es, {M, NewWs}));
                        [_|_] ->
                            self() ! {'begin', N, []},
                            runtime_base:become(meta_group(Qs, Fs, Ss, Cs, NewLs, Es, {M, NewWs}))
                    end
            end;
                                                % [TODO] add set_context, new to prevent blocking.
        {become, N, F, From, _} ->
            From ! end_become,
            runtime_base:become(meta_group(Qs, subst_nth(N, F, Fs), Ss, Cs, Ls, Es, Rb));
        _ -> 
            %% io:fwrite("atomawasi~n"),
            %% timer:sleep(10),
            self() ! RawM, % process later
            runtime_base:become(meta_group(Qs, Fs, Ss, Cs, Ls, Es, Rb))
    end.

%%%=========================================================================
%%%  Rollback Routines
%%%=========================================================================

% メッセージEに関連する全てのメッセージを取り消す。
% その中にまだ実行中のアクターが存在した場合、
% 取り消すべきメッセージを決定できないので待機リストを返す。
% collect_messages_to_cancel/5が処理のメイン。
cancel_messages_after({N, E}, Qs, Fs, Cs, Ls) ->
    [MesgsToCancel, MesgsToQueue, WaitingActors] = collect_messages_to_cancel([{N, E}], ordsets:new(), ordsets:new(), [], Ls),
    if 
        length(WaitingActors) > 0 -> {waiting, WaitingActors};
        true -> 
            [LsToCancel, LsToQueue] = [make_partial_log_list(List, Ls) || List <- [MesgsToCancel, MesgsToQueue]],
            [
             [lists:map(fun(_E)-> log:message(_E) end, L) ++ Q       || {Q, L} <- lists:zip(Qs, LsToQueue)],
             [case L of [H|T] -> log:before_function(H); [] -> F end || {F, L} <- lists:zip(Fs, LsToCancel)],
             [case L of [H|T] -> log:before_context(H);  [] -> C end || {C, L} <- lists:zip(Cs, LsToCancel)],
             make_removed_log_list(MesgsToCancel, Ls)
            ]
    end.

collect_messages_to_cancel([], Checked, Derived, Ws, Ls) ->
    [Checked, ordsets:subtract(Checked, Derived), Ws];
collect_messages_to_cancel([{N,E}|UnChecked], Checked, Derived, Ws, Ls) ->
    MesgsAfterE1 = [{N, _E} || _E <- lists:takewhile(fun(_E) -> log:message_ID(_E) /= log:message_ID(E) end, nth(N, Ls))],
    SentMesgsOfE0 = [{_N, log:lookup(_ID, nth(_N, Ls))} || {_N, _ID} <- log:sent_message_ID_and_dest_numbers(E)],
    %% まだキューから出されてないメッセージはログに無いはず(=not_found)なので除外
    SentMesgsOfE1 = remove_not_found_messages(SentMesgsOfE0),
    %% キューから出されたが、まだ処理中のメッセージを除外し、待機リストに追加
    {MesgsAfterE, Ws1}  = extract_processing_messages(MesgsAfterE1),
    {SentMesgsOfE, Ws2} = extract_processing_messages(SentMesgsOfE1),

    NewWs = remove_duplicate(Ws ++ Ws1 ++ Ws2),

    collect_messages_to_cancel([{_N, _E} || {_N, _E} <- MesgsAfterE ++ SentMesgsOfE, not ordsets:is_element({N, log:message_ID(_E)}, Checked)] ++ UnChecked, 
                               ordsets:add_element({N, log:message_ID(E)}, Checked), 
                               add_elements([{_N, log:message_ID(_E)} || {_N, _E} <- SentMesgsOfE], Derived),
                               NewWs, Ls).

%%%=========================================================================
%%%  Sub-Routines
%%%=========================================================================

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

remove_not_found_messages(Mesgs) ->
  lists:filter(fun ({_N, _E}) -> _E /= not_found end, Mesgs).

extract_processing_messages(Mesgs) ->
  lists:foldr(fun ({_N, _E}, {Es,Ws}) -> 
                  case log:processing(_E) of 
                    true  -> {Es, [_N|Ws]};
                    false -> {[{_N,_E}|Es], Ws}
                  end end, {[],[]}, Mesgs).

gen_ID() -> 
    base64:encode(crypto:strong_rand_bytes(4)).

get_ID_for_debug() ->
    case whereis(id_server) of
        undefined -> register(id_server,spawn(runtime_gwrc, id_server_for_debug, [0]));
        _ -> ok
    end,
    id_server ! {new_id, self()},
    receive
        {new_id, ID} -> ID
    end.

id_server_for_debug(N) ->
    receive 
        {new_id, From} -> 
            From ! {new_id, lists:concat([mmid,N])},
            id_server_for_debug(N+1)
    end.
