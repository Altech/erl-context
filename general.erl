-module(general).
-export([l/1, l/2, my_time/0]).

l(Format) ->
    l(Format, []).

l(Format, Args) ->
    io:fwrite(lists:concat(["[~s] " , Format, "~n"]), [my_time()|Args]).

my_time() ->
    {H, M, S} = time(),
    [$0 + H div 10, $0 + H rem 10, $:, $0 + M div 10, $0 + M rem 10, $:, $0 + S div 10, $0 + S rem 10].
