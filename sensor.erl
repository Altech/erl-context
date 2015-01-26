-module(sensor_core).
-export([main/1, setup/0, node_behavior/3, name_behavior/1]).
-include("core.hrl").

main([_]) ->
    timer:sleep(200),
    io:fwrite("~n"),
    l("setup: started",[]),
    Pids = setup(),
    l("setup: finished",[]),
    broadcast(Pids, get_sensor_data),
    receive 
        Result -> 
            l("main received ~p",[Result]),
            io:fwrite("1> ")
    end.

broadcast(Pids, Msg) ->
    lists:foreach(fun (Pid) ->
                          Pid ! Msg
                  end, Pids).

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
    NameToPid = lists:map(fun (Node) -> 
                                      Children = get_children(Node, Edges),
                                      Parent   = get_parent(Node, Edges),
                                      Pid = spawn(sensor_core, node_behavior, [Children, Parent, []]),
                                      {Node, Pid}
                              end, Nodes),
    % ネームサーバー（デバッグ用）
    register(name_server, spawn(sensor_core, name_behavior, [NameToPid])),
    % 各アクターの持つネットワーク参照を名前からアドレスに変更
    Pids = maps:values(maps:from_list(NameToPid)),
    lists:foreach(fun (Pid) ->
                          Pid ! {setup, self(), maps:from_list([{'node-1', self()}|NameToPid])},
                          receive 
                              finish_setup -> ok;
                              X -> error(X)
                          end
                  end, Pids),
    % 実験用乱数生成
    {A, B, C} = now(),
    random:seed(A,B,C),
    Pids.

self_name() ->
    node_name(self()).

node_name(Pid) ->
    name_server ! {Pid, self()},
    receive
        {name_server_reply, Name} -> Name
    end.

% Private Functions %
node_behavior(Children, Parent, ActorsAndVaues) ->
    receive
        {setup, From, NameToPid} ->
            [ParentPid|ChildrenPid] = 
                lists:map(fun (Child) -> maps:get(Child, NameToPid) end, [Parent|Children]),
            {A, B, C} = now(), random:seed(A,B,C),
            From ! finish_setup,
            node_behavior(ChildrenPid, ParentPid, ActorsAndVaues);
        get_sensor_data -> % Broadcasted
            l("~p received ~p", [self_name(), get_sensor_data]),
            l("~p major ~p times", [self_name(), (((self_local_number() rem 3) + 1)*100000)]),
            Value = major(((self_local_number() rem 3) + 1)*1000000),
            l("~p majored", [self_name()]),
            NewActorsAndVaues = [{self(), Value}|ActorsAndVaues],
            case is_completed([self()|Children], NewActorsAndVaues) of
                true ->
                    Parent ! {sensor,self(), summarize(NewActorsAndVaues)},
                    node_behavior(Children, Parent, []);
                false ->
                    node_behavior(Children, Parent, NewActorsAndVaues)
           end;
        {sensor, From, Value} -> 
            % [TODO] Check whether sender is a children or not
            NewActorsAndVaues = [{From, Value}|ActorsAndVaues],
            l("~p received ~p", [self_name(), {sensor, node_name(From), Value}]),
            case is_completed([self()|Children], NewActorsAndVaues) of
                true ->
                    Parent ! {sensor,self(), summarize(NewActorsAndVaues)},
                    node_behavior(Children, Parent, []);
                false ->
                    node_behavior(Children, Parent, NewActorsAndVaues)
           end
        end.

name_behavior(NameToPid) ->
    PidToName = maps:from_list(lists:map(fun ({Name,Pid}) -> 
                                                 {Pid, Name} 
                                         end, NameToPid)),
    name_behavior_iter(PidToName).

name_behavior_iter(PidToName) ->
    receive
        {Pid, From} -> 
            From ! {name_server_reply, maps:get(Pid, PidToName)},
            name_behavior_iter(PidToName);
        X ->
            error({unknown_message, X})
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

is_completed(Actors, ActorsAndVaues) ->
    ReceivedActors = maps:keys(maps:from_list(ActorsAndVaues)),
    lists:all(fun (Actor) ->
                      lists:member(Actor , ReceivedActors)
              end, Actors).

self_local_number() ->
    [_,S,_] = string:tokens(pid_to_list(self()),"."),
    {I,_} = string:to_integer(S),
    I.

% Computation 
summarize(ActorsAndVaues) ->
    Sum = lists:foldr(fun (Value, Sum) ->
                              Sum + Value
                      end, 
                      0, maps:values(maps:from_list(ActorsAndVaues))),
    Sum / length(ActorsAndVaues).

major() ->
    random:uniform(round(25+(self_local_number()/5))).

major(N) ->
    Sum = lists:foldr(fun (_, Sum) ->
                              major() + Sum
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
