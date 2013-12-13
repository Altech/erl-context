-module(context).
-export([new/1, default/0, value/1, compare/2]).

%% Context ::= {'$context', {Value, Timestamp}}

new(V) ->
    {'$context', {V, erlang:now()}}.

default() ->
    {'$context', {none, {0,0,0}}}.

value({V, T}) -> V.

compare({'$context', C1}, {'$context', C2}) ->
    compare(C1, C2);
compare({_, {MegaS1, S1, MicroS1}}, {_, {MegaS2, S2, MicroS2}}) ->
    MegaR = compare(MegaS1, MegaS2),
    if MegaR == same -> 
	    R = compare(S1, S2),
	    if R == same ->
		    compare(MicroS1, MicroS2);
	       true -> R
	    end;
       true -> MegaR
    end;
compare(I1, I2) ->
    if I1 > I2 -> older;
       I1 == I2 -> same;
       true -> newer
    end.
