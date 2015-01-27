-module(sensor_gwrc).
-export([main/1, setup/0]).
-include("runtime_gwrc.hrl").
-include("sensor.hrl").

%%%=========================================================================
%%%  Setup
%%%=========================================================================

main([_]) ->
    timer:sleep(200),
    io:fwrite("~n"),
    l("setup: started",[]),
    Root = setup(),
    l("setup: finished",[]),
    ?send(Root, start_with_context).

setup() ->
    % 各ノードに相当するアクターを立ちあげ
    [Nodes, Edges] = get_network_data(),
    Fs = map(fun (Node) -> 
                           I = get_index_from_node(Node),
                           Children = get_children(Node, Edges),
                           Parent   = get_parent(Node, Edges),
                           node_behavior(I, Children, Parent, [])
                   end, Nodes),
    G = ?new_group([root_behavior([])|Fs]),
    register(meta, G), % for debug
    NameToAddrSansRoot = map(fun (Node) -> 
                                     I = get_index_from_node(Node),
                                     {Node, {I+1, G}}
                             end, Nodes),
    RootAddr = {1, G},
    NameToAddr = [{'node0', RootAddr}|NameToAddrSansRoot],
    Addrs = maps:values(maps:from_list(NameToAddr)),
    % 各アクターの持つネットワーク参照を名前からアドレスに変更
    foreach(fun (Addr) ->
                    Map = maps:from_list(NameToAddr),
                    ?send(Addr, {setup, self(), Map}),
                    receive 
                        {mesg, finish_setup, _} -> ok;
                        X -> error(X)
                    end
            end, Addrs),
    RootAddr.

%%%=========================================================================
%%%  Behaviors
%%%=========================================================================

root_behavior(Addrs) ->
    fun (Msg) -> 
            case Msg of 
                {setup, From, Map} ->
                    AddrsSansSelf = maps:values(maps:remove('node0', Map)),
                    ?send(From, finish_setup),
                    ?become(root_behavior(AddrsSansSelf));
                start ->
                    ?set_context(arithmetic),
                    broadcast(Addrs, get_sensor_data),
                    ?become(root_behavior(Addrs));
                start_with_context ->
                    ?set_context(arithmetic),
                    broadcast(Addrs, get_sensor_data),
                    ?send(?self(), next_context),
                    ?become(root_behavior(Addrs));
                next_context ->
                    ?set_context(geometricc),
                    broadcast(Addrs, get_sensor_data),
                    ?become(root_behavior(Addrs));
                _ ->
                    l("root received ~p",[Msg])
            end
    end.

node_behavior(I, Children, Parent, ActorsAndVaues) ->
    fun (Msg) ->
            case Msg of 
                {setup, From, NameToAddr} ->
                    %% l("~p received ~p", [self_name(), {setup, From, NameToAddr}]),
                    [ParentAddr|ChildrenAddr] = 
                        map(fun (Child) -> 
                                    maps:get(Child, NameToAddr) 
                            end, [Parent|Children]),
                    {A, B, C} = now(), random:seed(A,B,C),
                    ?send(From, finish_setup),
                    ?become(node_behavior(I, ChildrenAddr, ParentAddr, ActorsAndVaues));
                get_sensor_data ->
                    l("(~p) ~p received ~p", [?self_context(), self_name(), get_sensor_data]),
                    %% l("~p major ~p times", [?self_context(), self_name(), (((I rem 3) + 1)*1000000)]),
                    Value = major(?MAJOR_TIMES(I), I),
                    %% l("~p majored", [self_name()]),
                    NewActorsAndVaues = [{?self(), Value}|ActorsAndVaues],
                    node_behavior_rest(I, Children, Parent, NewActorsAndVaues);
                {sensor, From, Value} ->
                    NewActorsAndVaues = [{From, Value}|ActorsAndVaues],
                    l("(~p) ~p received ~p", [?self_context(), self_name(), {sensor, node_name(From), Value}]),
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

self_name() ->
    node_name(?self()).

node_name(Addr) ->
    case Addr of
        {N,_} -> list_to_atom(lists:concat([node,N-1]));
        _ -> Addr
    end.

%%%=========================================================================
%%%  Computation
%%%=========================================================================

summarize(ActorsAndVaues) ->
    case ?self_context() of
        arithmetic -> 
            %% l("arithmetic mean!"),
            Sum = foldr(fun (Value, Sum) ->
                                Sum + Value
                        end, 
                        0, maps:values(maps:from_list(ActorsAndVaues))),
            Sum / length(ActorsAndVaues);
        geometricc ->
            Sum = foldr(fun (Value, Sum) ->
                                Sum * Value
                        end, 
                        1, maps:values(maps:from_list(ActorsAndVaues))),
            %% l("geometricc mean!"),
            general:nth_root(length(ActorsAndVaues), Sum)
    end.

major(I) -> % Iはセンサーの値の期待値を変更するため
    random:uniform(round(25+(I/5))).

major(N,I) ->
    Sum = foldr(fun (_, Sum) ->
                        major(I) + Sum
                end,
                0 , lists:seq(1,N)),
    Sum / N.
