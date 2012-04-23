-module(beset).


-export([init/1,loop/1,subsets/2, update_set/3, delete_set/2, add_set/2, get_set/2, generate_key/0, set_to_json/1, list_to_json/1, json_to_list/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.




init(Port)->
    io:fwrite("~ninit w:~p~n", [Port]),

    GetFunction=fun(Sock, PathString, QueryString, Params, Fragment, Headers, Body, Pid)-> 
			io:fwrite("~nparams:~p~n", [Params]),
			io:fwrite("~npath:~p~n", [PathString]),

			[_|SetId]=PathString,
			case PathString of
			    "/subsets" -> {"superset", SupersetJson}=lists:keyfind("superset", 1, Params),
					  io:format("json: ~p~n", [SupersetJson]),
					  SupersetList=json_to_list(SupersetJson),
					  io:format("list: ~p~n", [SupersetList]),
					  SupersetSet=sets:from_list(SupersetList),
					  io:format("set: ~p~n", [SupersetSet]),
					  Pid ! {subset_query, SupersetSet, self()},
					  
					 receive
					     %%todo: collapse map into foldl
					     {ok, Subsets}->F=fun(Subset)->
								      {Id, Set}=Subset,lists:flatten(["{\"", Id, "\": ", set_to_json(Set), "}"]) 
							      end,
							    X=lists:map(F, Subsets),
							    io:format("~p~n", [X]),
							    Response=lists:flatten(["[" , string:join(X, ","),"]"]),
							    simpleservice:send_message(Sock, Response, "application/json", 200, "Subsets");
					     X -> io:format("~p~n", [X]), simpleservice:send_message(Sock, "An error has occurred!", "text/plain", 500, "Internal Server Error")			 
					 end;
			    _ ->
			    
				Pid ! {get, SetId, self()},
				receive
				    {ok, Set} -> io:fwrite("~nset:~p~n", [Set]), simpleservice:send_message(Sock, ?MODULE:set_to_json(Set), "application/json", 200, "OK");
				    _  -> simpleservice:send_message(Sock, "An error has occurred!", "text/plain", 500, "Internal Server Error")			 
				end
			end
		end,

    DeleteFunction=fun(Sock, PathString, QueryString, Params, Fragment, Headers, Body, Pid)-> 
			   [_|SetId]=PathString,
			   Pid ! {delete, SetId, self()},
			   receive
			       ok -> simpleservice:send_message(Sock, nil, "text/plain", 204, "No Content");
			       _  -> simpleservice:send_message(Sock, "An error has occurred!", "text/plain", 500, "Internal Server Error")
			   end
		   end,

    PostFunction=fun(Sock, PathString, QueryString, Params, Fragment, Headers, Body, Pid)-> 
			 Pid ! {post, sets:from_list(?MODULE:json_to_list(Body)), self()},
			 receive
			     {ok, SetId} -> simpleservice:send_message(Sock, SetId, "text/plain", 200, "OK");
			     _  -> simpleservice:send_message(Sock, "An error has occurred!", "text/plain", 500, "Internal Server Error")			 
			 end
		 end,
    
    Functions=[{'GET', GetFunction}, {'DELETE', DeleteFunction}, {'POST', PostFunction}],
    
    SetDb=[],
    Pid=spawn(?MODULE, loop, [SetDb]),
    simpleservice:start(Port, Functions, Pid),
    {ok, running}.

loop(SetDb)->
    receive
	{get, SetId, Pid} ->
	    Result=get_set(SetId, SetDb),
	    Pid ! Result,
	    ?MODULE:loop(SetDb);
	{subset_query, Superset, Pid} ->
	    Result=subsets(Superset, SetDb),
	    Pid ! {ok, Result},
	    ?MODULE:loop(SetDb);
	{post, Set, Pid}->
	    io:fwrite("in loop add", []),
	    {ok, SetId, NewSetDb}=add_set(Set, SetDb),
	    Pid ! {ok, SetId},
	    ?MODULE:loop(NewSetDb);
	{delete, SetId, Pid} ->
	    {ok, NewSetDb}=delete_set(SetId, SetDb),
	    Pid ! ok,
	    ?MODULE:loop(NewSetDb);
	{put, SetId, Set, Pid} ->
	    {ok, NewSetDb}=update_set(SetId, Set, SetDb),
	    Pid ! ok,
	    ?MODULE:loop(NewSetDb);
	{_, Pid} ->
	    Pid ! {err, no_such_method},
	    ?MODULE:loop(SetDb)
    end.
    
	    

    

 %% @doc Returns a list of all sets in the store that are subsets of the goven set.

get_set(SetId, SetDb)->
    
    Result=lists:keyfind(SetId, 1, SetDb),
    case Result of 
	{SetId, Set}->
	    {ok, Set};
	_ ->false
    end. 





 %% @doc Returns a list of all sets in the store that are subsets of the given set.

subsets(Set, SetDb)->
    lists:filter(fun(X)->{Id, StoredSet}=X,sets:is_subset(StoredSet, Set) end, SetDb).


 %% @doc Updates/replaces the given set as idientified by the key with the contents of Set.

update_set(Key, Set, SetDb)->
    io:fwrite("in update", []),
    NewDb=lists:keystore(Key, 1, SetDb, {Key,Set}),
    {ok, Key, NewDb}.

 %% @doc Removes specified set from the store.

delete_set(Key, SetDb)->
    io:fwrite("key to delete: ~p~n", [Key]),
    NewSetDb=lists:keydelete(Key, 1, SetDb),
    {ok, NewSetDb}.



 %% @doc Adds a set to the store.

add_set(Set, SetDb)->
    ?MODULE:update_set(generate_key(), Set, SetDb).





%% @doc create a unique id/key
generate_key()->
    {A,B,C}=now(),
    <<Y:128>>=erlang:md5(atom_to_list(node())), lists:flatten(io_lib:format("~32.16.0b", [Y])),
        lists:flatten([io_lib:format("~p",[Y]),io_lib:format("~p", [A]),io_lib:format("~p", [B]),io_lib:format("~p", [C])]).



%% @doc serialize erlang set/term to JSON
set_to_json(Set)->
    SetList=sets:to_list(Set),
    io:fwrite("~p~n", [SetList]),
    list_to_json(SetList).


%% @doc serialize erlang set/term to JSON
list_to_json(List)->
    list_to_json(List, ["["]).
list_to_json([H|T], Acc) ->
    Item=lists:flatten(["\"", H, "\""]),
    case length(T) of
	0->list_to_json(T, lists:append([Acc,  Item]));
	_ ->list_to_json(T, lists:append([Acc, Item, ", "]))
    end;
list_to_json([], Acc) ->
    lists:flatten([Acc, "]"]).
    
    

%% @doc read erlang
json_to_list(Json)->
    string:tokens(Json, "[],\" ").


    


-ifdef(TEST).

set_to_json_test()->
    OneTwo=sets:add_element("1", sets:add_element("2", sets:new())),
    X=set_to_json(OneTwo),
    ?assertEqual(lists:sort("[\"1\", \"2\"]"),lists:sort(X)).



json_to_list_test()->
    Json="[\"Hello\", \"World\", \"Bar\", \"Foo\"]",
    List=["Hello", "World", "Bar", "Foo"],
    ?assertEqual(List,json_to_list(Json)).

list_to_json_test()->
    List=["Hello", "World", "Bar", "Foo"],
    ?assertEqual("[\"Hello\", \"World\", \"Bar\", \"Foo\"]",list_to_json(List)).
    
add_set_test()->
    RedBlue=sets:add_element(blue, sets:add_element(red, sets:new())),
    {ok,Key, Db}=add_set(RedBlue,[]),
    ?assertEqual(1, length(Db)).

delete_set_test()->
    RedBlue=sets:add_element(blue, sets:add_element(red, sets:new())),
    YellowGreen=sets:add_element(yellow, sets:add_element(green, sets:new())),
    OrangeBlue=sets:add_element(orange, sets:add_element(blue, sets:new())),
    RedBlueYellowGreen=sets:add_element(green, sets:add_element(yellow, sets:add_element(blue, sets:add_element(red, sets:new())))),
    TestData=[{red_blue, RedBlue}, {yellow_green, YellowGreen}, {orange_blue, OrangeBlue}],
    ?assertEqual(3, length(TestData)),
    {ok, Results}=?MODULE:delete_set(red_blue, TestData),
    ?assertEqual(2, length(Results)).



subsets_test()->
    RedBlue=sets:add_element(blue, sets:add_element(red, sets:new())),
    YellowGreen=sets:add_element(yellow, sets:add_element(green, sets:new())),
    OrangeBlue=sets:add_element(orange, sets:add_element(blue, sets:new())),
    RedBlueYellowGreen=sets:add_element(green, sets:add_element(yellow, sets:add_element(blue, sets:add_element(red, sets:new())))),
    TestData=[{red_blue, RedBlue}, {yellow_green, YellowGreen}, {orange_blue, OrangeBlue}],
    Results=?MODULE:subsets(RedBlueYellowGreen, TestData),
    ?assertEqual(2, length(Results)).

-endif.
