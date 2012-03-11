-module(beset).


-export([init/1,subsets/1, update_set/2, delete_set/1, add_set/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.




init(Port)->
    io:fwrite("~ninit w:~p~n", [Port]),
    GetFunction=fun(Sock, PathString, QueryString, Params, Fragment, Headers, Body)-> 
			io:fwrite("~nparams:~p~n", [Params]),
			
			gen_tcp:send(Sock, "HTTP/1.1 200 OK\r\nContent-Type: text/plain; charset=UTF-8\r\nConnection: close\r\n\r\nHelo World!\r\n\r\n") 
		end,
    Functions=[{'GET', GetFunction}],

simpleservice:start(Port, Functions),
    {ok, running}.

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


-ifdef(TEST).

simple_test() ->
        ok.% = init(9999).
-endif.
