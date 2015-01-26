-module(sensor_core).
-export([main/1, setup/0]).
-include("core.hrl").
-import(general, [l/1, l/2, my_time/0]).
-import(data, [get_network_data/0, get_children/2, get_parent/2, get_index_from_node/1]).
-import(name_server, [setup/1, self_name/0, node_name/1]).

-define(MAJOR_TIMES(I), ((I rem 3) + 1)*1000000).

%%%=========================================================================
%%%  Setup
%%%=========================================================================

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
    name_server:setup(NameToAddr),
    % 各アクターの持つネットワーク参照を名前からアドレスに変更
    Addrs = maps:values(maps:from_list(NameToAddr)),
    lists:foreach(fun (Addr) ->
                          Map = maps:from_list([{'node-1', self()}|NameToAddr]),
                          ?send(Addr, {setup, self(), Map}),
                          receive 
                              finish_setup -> ok;
                              X -> error(X)
                          end
                  end, Addrs),
    Addrs.

%%%=========================================================================
%%%  Behaviors
%%%=========================================================================

node_behavior(I, Children, Parent, ActorsAndVaues) ->
    fun (Msg) ->
            case Msg of 
                {setup, From, NameToAddr} ->
                    [ParentAddr|ChildrenAddr] = 
                        lists:map(fun (Child) -> 
                                          maps:get(Child, NameToAddr) 
                                  end, [Parent|Children]),
                    {A, B, C} = now(), random:seed(A,B,C),
                    ?send(From, finish_setup),
                    ?become(node_behavior(I, ChildrenAddr, ParentAddr, ActorsAndVaues));
                get_sensor_data ->
                    l("~p received ~p", [self_name(), get_sensor_data]),
                    l("~p major ~p times", [self_name(), (((I rem 3) + 1)*1000000)]),
                    Value = major(?MAJOR_TIMES(I), I),
                    l("~p majored", [self_name()]),
                    NewActorsAndVaues = [{?self(), Value}|ActorsAndVaues],
                    node_behavior_rest(I, Children, Parent, NewActorsAndVaues);
                {sensor, From, Value} ->
                    NewActorsAndVaues = [{From, Value}|ActorsAndVaues],
                    l("~p received ~p", [self_name(), {sensor, node_name(From), Value}]),
                    node_behavior_rest(I, Children, Parent, NewActorsAndVaues)
            end
    end.

node_behavior_rest(I, Children, Parent, NewActorsAndVaues) ->
    case is_completed([?self()|Children], NewActorsAndVaues) of
        true ->
            ?send(Parent, {sensor, ?self(), summarize(NewActorsAndVaues)}),
            ?become(node_behavior(I, Children, Parent, []));
        false ->
            ?become(node_behavior(I, Children, Parent, NewActorsAndVaues))
    end.

%%%=========================================================================
%%%  Sub-Routines
%%%=========================================================================

is_completed(Actors, ActorsAndVaues) ->
    ReceivedActors = maps:keys(maps:from_list(ActorsAndVaues)),
    lists:all(fun (Actor) ->
                      lists:member(Actor , ReceivedActors)
              end, Actors).

%%%=========================================================================
%%%  Computation
%%%=========================================================================

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
