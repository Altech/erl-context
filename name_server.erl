-module(name_server).
-include("core.hrl").
-export([setup/1, self_name/0, node_name/1]).
-export([name_behavior_init/1]).

setup(NameToAddr) ->
    ServerPid = ?new(name_behavior_init(NameToAddr)),
    register(name_server, ServerPid).

name_behavior_init(NameToAddr) ->
    AddrToName = maps:from_list(lists:map(fun ({Name,Addr}) -> 
                                                 {Addr, Name}
                                          end, NameToAddr)),
    name_behavior(AddrToName).

name_behavior(AddrToName) ->
    fun (X) ->
            case X of 
                {Addr, From} ->
                    From ! {name_server_reply, maps:get(Addr, AddrToName)},
                    ?become(name_behavior(AddrToName));
                X -> 
                    io:fwrite("name_server unknown : ~p", [X]),
                    ?become(name_behavior(AddrToName))
            end
    end.

self_name() ->
    node_name(?self()).

node_name(Addr) ->
    name_server ! {Addr, ?self()},
    receive
        {name_server_reply, Name} -> Name
    end.
