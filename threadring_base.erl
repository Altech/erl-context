-module(threadring_base).
-export([main/1, roundtrip_gen/2]).

-include("runtime_base.hrl").

start(Token) ->
   H = lists:foldl(
      fun(Id, Pid) -> ?new(roundtrip_gen(Id, Pid)) end,
      self(), 
      lists:seq(?RING, 2, -1)),
   ?send(H, Token),
   ?become(roundtrip_gen(1, H)).

roundtrip_gen(Id, Pid) ->
    fun (Token) ->
            case Token of
                1 -> io:fwrite("~b~n", [Id]),
                     erlang:halt();
                _ -> ?send(Pid, Token - 1),
                     ?become(roundtrip_gen(Id, Pid))
            end
    end.

main([Arg]) ->
   Token = list_to_integer(Arg),
   start(Token).
