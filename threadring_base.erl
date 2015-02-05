-module(threadring_base).
-export([main/1, roundtrip_gen/2]).

-include("runtime_base.hrl").

start(Token) ->
   H = lists:foldl(
      fun(Id, Pid) -> ?new(roundtrip_gen(Id, Pid)) end,
      self(), 
      lists:seq(?RING, 2, -1)),
  lists:foreach(fun (_) -> 
                    ?send(H, Token)
                end, lists:seq(1, ?MESSAGES)),
  ?become(roundtrip_gen(1, H)).

roundtrip_gen(Id, Pid) ->
  fun (Token) ->
      case Token of
        1 -> io:fwrite("count:~p, id:~b~n", [get(count), Id]),
             N = case get(count) of 
                   undefined -> 1;
                   M -> M+1
                 end,
             case N < ?MESSAGES of
               true -> put(count,N);
               false -> erlang:halt()
             end;
        _ -> ?send(Pid, Token - 1)
      end,
      ?become(roundtrip_gen(Id, Pid))
  end.

main([Arg]) ->
   Token = list_to_integer(Arg),
   start(Token).
