-module(sensor_core).
-export([main/1, setup/0]).
-include("core.hrl").

main([_]) ->
    timer:sleep(200),
    io:fwrite("~n"),
    l("setup: started",[]),
    Addrs = setup(),
    l("setup: finished",[]),
    broadcast(Addrs, get_sensor_data),
    receive 
        Result -> 
            l("main received ~p",[Result]),
            io:fwrite("1> ")
    end.

broadcast(Addrs, Msg) ->
    lists:foreach(fun (Addr) ->
                          ?send(Addr,Msg)
                  end, Addrs).

% Return registered process names
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

setup() ->
    % 各ノードに相当するアクターを立ちあげ
    [Nodes, Edges] = get_network_data(),
    NameToAddr = lists:map(fun (Node) -> 
                                   I = get_index_from_node(Node),
                                   Children = get_children(Node, Edges),
                                   Parent   = get_parent(Node, Edges),
                                   Addr = ?new(node_behavior(I, Children, Parent, [])),
                                   {Node, Addr}
                          end, Nodes),
    % ネームサーバー（デバッグ用）
    register(name_server, ?new(name_behavior_init(NameToAddr))),
    % 各アクターの持つネットワーク参照を名前からアドレスに変更
    Addrs = maps:values(maps:from_list(NameToAddr)),
    lists:foreach(fun (Addr) ->
                          ?send(Addr, {setup, self(), maps:from_list([{'node-1', self()}|NameToAddr])}),
                          receive 
                              finish_setup -> ok;
                              X -> error(X)
                          end
                  end, Addrs),
    % 実験用乱数生成
    {A, B, C} = now(),
    random:seed(A,B,C),
    Addrs.

self_name() ->
    node_name(?self()).

node_name(Addr) ->
    ?send(name_server, {Addr, ?self()}),
    receive
        {name_server_reply, Name} -> Name
    end.

% Private Functions %
node_behavior(I, Children, Parent, ActorsAndVaues) ->
    fun (Msg) ->
            case Msg of 
                {setup, From, NameToAddr} ->
                    [ParentAddr|ChildrenAddr] = 
                        lists:map(fun (Child) -> maps:get(Child, NameToAddr) end, [Parent|Children]),
                    {A, B, C} = now(), random:seed(A,B,C),
                    ?send(From, finish_setup),
                    ?become(node_behavior(I, ChildrenAddr, ParentAddr, ActorsAndVaues));
                get_sensor_data ->
                    l("~p received ~p", [self_name(), get_sensor_data]),
                    l("~p major ~p times", [self_name(), (((I rem 3) + 1)*1000000)]),
                    Value = major(((I rem 3) + 1)*1000000, I),
                    l("~p majored", [self_name()]),
                    NewActorsAndVaues = [{?self(), Value}|ActorsAndVaues],
                    case is_completed([?self()|Children], NewActorsAndVaues) of
                        true ->
                            ?send(Parent, {sensor, ?self(), summarize(NewActorsAndVaues)}),
                            ?become(node_behavior(I, Children, Parent, []));
                        false ->
                            ?become(node_behavior(I, Children, Parent, NewActorsAndVaues))
                    end;
                {sensor, From, Value} -> 
                    % [TODO] Check whether sender is a children or not
                    NewActorsAndVaues = [{From, Value}|ActorsAndVaues],
                    l("~p received ~p", [self_name(), {sensor, node_name(From), Value}]),
                    case is_completed([?self()|Children], NewActorsAndVaues) of
                        true ->
                            ?send(Parent, {sensor, ?self(), summarize(NewActorsAndVaues)}),
                            ?become(node_behavior(I, Children, Parent, []));
                        false ->
                            ?become(node_behavior(I, Children, Parent, NewActorsAndVaues))
                    end
            end
    end.

name_behavior_init(NameToAddr) ->
    AddrToName = maps:from_list(lists:map(fun ({Name,Addr}) -> 
                                                 {Addr, Name} 
                                         end, NameToAddr)),
    name_behavior(AddrToName).

name_behavior(AddrToName) ->
    fun ({Addr, From}) ->
            ?send(From, {name_server_reply, maps:get(Addr, AddrToName)}),
            ?become(name_behavior(AddrToName))
    end.

% Sub-Functions %
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

is_completed(Actors, ActorsAndVaues) ->
    ReceivedActors = maps:keys(maps:from_list(ActorsAndVaues)),
    lists:all(fun (Actor) ->
                      lists:member(Actor , ReceivedActors)
              end, Actors).

% Computation 
summarize(ActorsAndVaues) ->
    Sum = lists:foldr(fun (Value, Sum) ->
                              Sum + Value
                      end, 
                      0, maps:values(maps:from_list(ActorsAndVaues))),
    Sum / length(ActorsAndVaues).

major(I) -> % Iはセンサーの値の期待値を変更するため
    random:uniform(round(25+(I/5))).

major(N,I) ->
    Sum = lists:foldr(fun (_, Sum) ->
                              major(I) + Sum
                      end,
                      0 , lists:seq(1,N)),
    Sum / N.

% General Functions %
l(Format) ->
    l(Format, []).
l(Format, Args) ->
    io:fwrite(lists:concat(["[~s] " , Format, "~n"]), [my_time()|Args]).

my_time() ->
    {H, M, S} = time(),
    [$0 + H div 10, $0 + H rem 10, $:, $0 + M div 10, $0 + M rem 10, $:, $0 + S div 10, $0 + S rem 10].

