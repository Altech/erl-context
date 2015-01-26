-module(threadring_runtime_ctx).
-export([main/1, roundtrip_gen/1]).

-include("runtime_ctx_opt.hrl").

start(Token) ->
   Fs = lists:foldl(
      fun(Id, Fs) -> [roundtrip_gen(Id)|Fs] end,
      [], 
      lists:seq(?RING, 1, -1)),
   G = ?new_group(Fs),
   ?send({1, G}, Token),
   timer:sleep(1000000).

roundtrip_gen(?RING) ->
    fun (Token) ->
            case Token of
                1 -> io:fwrite("~b~n", [?RING]),
                     erlang:halt();
                _ -> ?send(?neighbor(1), Token - 1)
            end
    end;
roundtrip_gen(Id) ->
    fun (Token) ->
            case Token of
                1 -> io:fwrite("~b~n", [Id]),
                     erlang:halt();
                _ -> ?send(?neighbor(Id+1), Token - 1)
            end
    end.

main([Arg]) ->
   Token = list_to_integer(Arg),
   start(Token).
