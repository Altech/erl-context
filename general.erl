-module(general).
-export([nth/2, subst_nth/3, replicate/2, element_after/2, tuple_reverse/1, add_elements/2, l/1, l/2, my_time/0]).

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

tuple_reverse({E1, E2}) ->
    {E2, E1}.

add_elements(Ls, Set) ->
    lists:foldl(fun(E, Set)-> ordsets:add_element(E, Set) end, Set, Ls).

l(Format) ->
    l(Format, []).

l(Format, Args) ->
    io:fwrite(lists:concat(["[~s] " , Format, "~n"]), [my_time()|Args]).

my_time() ->
    {H, M, S} = time(),
    [$0 + H div 10, $0 + H rem 10, $:, $0 + M div 10, $0 + M rem 10, $:, $0 + S div 10, $0 + S rem 10].
