-module(runtime_gwr).
-export([new/1, send/2, become/1, new_group/1, usr_self/0, neighbor/1, send_delay/3]).
-import(general,[nth/2, subst_nth/3, replicate/2]).

%%%=========================================================================
%%%  API
%%%=========================================================================

new(F) ->
    case get(self) of
	undefined -> 
	    runtime_base:new(meta1([], F, dormant, runtime_base:new(fun exec/1)));
	{_, MetaG} ->
	    MetaG ! {new, F, self(), []},
	    runtime_base:become(fun(X) -> X end) % 返り値を使用（注：モデルからの逸脱）
    end.

new_group(Fs) ->
    N = length(Fs),
    runtime_base:new(meta_group(replicate(N, []), Fs, replicate(N, dormant), runtime_base:new(fun exec/1))).

become(F) ->
    case get(self) of
        undefined -> 
            runtime_base:become(F);
        {N, MetaG} ->
            % Erlang及びアクターモデルではメッセージの送受信順序は保証されているので、
            % たぶんこれで大丈夫。
            MetaG ! {become, N, F, []};
        _ ->
            runtime_base:become(F)
    end.

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

% For Experiments
send_delay(Dest, Msg, Delay) ->    
    spawn(fun() -> 
		  timer:sleep(Delay),
		  case Dest of 
		      {N, _Dest} -> _Dest ! {mesg, {N, Msg}, []};
		      _ -> Dest ! {mesg, Msg, []}
		  end
	  end).

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
	    From ! {'end', []},
	    runtime_base:become(fun exec/1);
	% From Group-Wide Meta-Level
	{apply, F, M, From, N} ->
	    put(self, {N, From}),
	    apply(F, [M]),
	    From ! {'end', N, []},
	    runtime_base:become(fun exec/1)    
    end.

meta1(Q, F, S, E) ->
    fun (RawM) ->
	case RawM of
	    {mesg, M, []} ->
		case S of
		    dormant ->
			self() ! {'begin', []},
			runtime_base:become(meta1(Q++[M], F, active, E));
		    active ->
			runtime_base:become(meta1(Q++[M], F, active, E))
		end;
	    {'begin', []} ->
		case Q of
		    [M|_Q] ->
			E ! {apply, F, M, self()},
			runtime_base:become(meta1(_Q, F, S, E))
		    end;
	    {'end', []} ->
		case Q of
		    [] -> runtime_base:become(meta1(Q, F, dormant, E));
		    [_|_] ->
			self() ! {'begin', []},
			runtime_base:become(meta1(Q, F, S, E))
		end
	end
    end.

meta_group(Qs, Fs, Ss, E) ->
  fun (RawM) ->
      case RawM of
	{mesg, {N, M}, _} ->
	  case nth(N, Ss) of
	    dormant ->
	      self() ! {'begin', N, []},
	      runtime_base:become(meta_group(subst_nth(N, nth(N,Qs)++[M], Qs), Fs, subst_nth(N, active, Ss), E));
	    active ->
	      runtime_base:become(meta_group(subst_nth(N, nth(N,Qs)++[M], Qs), Fs, subst_nth(N, active, Ss), E))
	  end;
	{'begin', N, _} ->
	  case nth(N, Qs) of
	    [M|_Q] ->
                  if M == 1 -> io:format("~p, ~n", [erlang:process_info(self(), memory)]);
                     true -> nil
                  end,
	      E ! {apply, nth(N, Fs), M, self(), N},
	      runtime_base:become(meta_group(subst_nth(N, _Q, Qs), Fs, Ss, E))
	  end;
	{'end', N, _} ->
	  case nth(N, Qs) of
	    [] -> runtime_base:become(meta_group(Qs, Fs, subst_nth(N, dormant, Ss), E));
	    [_|_] ->
	      self() ! {'begin', N, []},
	      runtime_base:become(meta_group(Qs, Fs, Ss, E))
	  end;
	{new, F, From, _} ->
	  N = length(Qs) + 1,
	  From ! {N, self()}, % これダメじゃね
	  runtime_base:become(meta_group(Qs++[[]], Fs++[F], Ss++[dormant], E));
        {become, N, F, _} ->
          runtime_base:become(meta_group(Qs, subst_nth(N, F, Fs), Ss, E));
	inspect -> % for debug
	  erlang:display({Qs, Fs, Ss}),
	  runtime_base:become(meta_group(Qs, Fs, Ss, E))
      end
  end.
