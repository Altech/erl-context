-import(lists,[foreach/2,map/2,foldr/3]).
-import(general, [l/1, l/2, my_time/0]).
-import(data, [get_network_data/0, get_children/2, get_parent/2, get_index_from_node/1]).

-define(MAJOR_TIMES(I), ((I rem 3) + 1)*1000000).
