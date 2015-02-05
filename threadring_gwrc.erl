-module(threadring_gwrc).
-export([main/1, roundtrip_gen/1]).

-include("runtime_gwrc.hrl").

start(Token) ->
  Fs = lists:foldl(
         fun(Id, Fs) -> [roundtrip_gen(Id)|Fs] end,
         [], 
         lists:seq(?RING, 1, -1)),
  G = ?new_group(Fs),
  lists:foreach(fun (_) -> 
                    ?send({1, G}, Token)
                end, lists:seq(1, ?MESSAGES)),
  timer:sleep(1000000).

roundtrip_gen(?RING) ->
  fun (Token) ->
      case Token of
        1 -> io:fwrite("count:~p, id:~b~n", [get(count), ?RING]),
             N = case get(count) of 
                   undefined -> 1;
                   M -> M+1
                 end,
             case N < ?MESSAGES of
               true -> put(count,N);
               false -> erlang:halt()
             end;
        _ -> ?send(?neighbor(1), Token - 1)
      end
  end;
roundtrip_gen(Id) ->
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
        _ -> ?send(?neighbor(Id+1), Token - 1)
      end
  end.

main([Arg]) ->
   Token = list_to_integer(Arg),
   start(Token).
