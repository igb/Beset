-module(beset).

-export([init/0, subsets/1, update_set/2, delete_set/1, add_set/2]).

init()->
    ok.

loop()->
    ok.

 %% @doc Returns a list of all sets in the store that are subsets of the goven set.
subsets(Set)->
    err.


 %% @doc Updates/replaces the given set as idientified by the key with the contents of Set.

update_set(Key, Set)->
    err.

 %% @doc Removes specified set from the store.

delete_set(Key)->
    err.

 %% @doc Adds a set to the store.

add_set(Key, Set)->
    err.
