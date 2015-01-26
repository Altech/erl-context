-module(threadring_erl).
-export([main/1, roundtrip/2]).

start(Token) ->
   H = lists:foldl(
      fun(Id, Pid) -> spawn(threadring_erl, roundtrip, [Id, Pid]) end, 
      self(), 
      lists:seq(?RING, 2, -1)),
   H ! Token,
   roundtrip(1, H).

roundtrip(Id, Pid) ->
   receive
      1 ->
         io:fwrite("~b~n", [Id]),
         erlang:halt();
      Token ->
         Pid ! Token - 1,
         roundtrip(Id, Pid)
   end.

main([Arg]) ->
   Token = list_to_integer(Arg),
   start(Token).
