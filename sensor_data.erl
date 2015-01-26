-module(sensor_data).
-export([get_network_data/0, get_children/2, get_parent/2, get_index_from_node/1]).

get_network_data() ->
    NumNodes = lists:seq(0,6),
    NumEdges = [[5,4],[6,4],[4,2],[3,2],[2,1],[1,0],[0,-1]],
    [
     lists:map(fun (NumNode) -> 
                       list_to_atom(lists:concat([node, NumNode])) 
               end, NumNodes),
     lists:map(fun (NumEdge) ->
                       [From, To] = NumEdge,
                       [
                        list_to_atom(lists:concat([node, From])), 
                        list_to_atom(lists:concat([node, To]))
                       ]
               end, NumEdges)
    ].

get_children(Node, Edges) ->
    lists:filtermap(fun([From, To]) -> 
                            if 
                                To == Node -> {true, From}; 
                                true -> false 
                            end
                    end, Edges).

get_parent(Node, Edges) ->
    [H|_] = lists:filtermap(fun([From, To]) -> 
                                    if 
                                        From == Node -> {true, To};
                                        true -> false
                                    end
                            end, Edges),
    H.

get_index_from_node(Node) ->
    [S|_] = io_lib:format("~p",[Node]),
    {I,_} = string:to_integer(string:substr(S,5)),
    I.
