-module(general).
-export([nth/2, subst_nth/3, replicate/2, element_after/2, tuple_reverse/1, add_elements/2, l/1, nth_root/2, l/2, my_time/0]).

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

nth_root(N, X) -> nth_root(N, X, 1.0e-5).
nth_root(N, X, Precision) ->
    F = fun(Prev) -> ((N - 1) * Prev + X / math:pow(Prev, (N-1))) / N end,
    fixed_point(F, X, Precision).

fixed_point(F, Guess, Tolerance) ->
    fixed_point(F, Guess, Tolerance, F(Guess)).
fixed_point(_, Guess, Tolerance, Next) when abs(Guess - Next) < Tolerance ->
    Next;
fixed_point(F, _, Tolerance, Next) ->
    fixed_point(F, Next, Tolerance, F(Next)).

l(Format) ->
    l(Format, []).

l(Format, Args) ->
    io:fwrite(lists:concat(["[~s] " , Format, "~n"]), [my_time()|Args]).

my_time() ->
    {H, M, S} = time(),
    [$0 + H div 10, $0 + H rem 10, $:, $0 + M div 10, $0 + M rem 10, $:, $0 + S div 10, $0 + S rem 10].
