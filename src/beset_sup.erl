
-module(beset_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, foo/1]).

%% Supervisor callbacks
-export([init/1]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% Helper macro for declaring children of supervisor
%-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Port) ->
    io:fwrite("hola", []),
    supervisor:start_link(beset_sup, [Port]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Port]) ->
    io:fwrite("hola2", []),
    {ok, 
     {{one_for_one, 5, 10}, 
     [{
       beset_sup, 
       {beset_sup, foo, [Port]}, 
       permanent, 
       brutal_kill, 
       worker,
       [beset_sup]
      }]
    }}.


foo(Port)->
     io:fwrite("port~p", [Port]),
    timer:sleep(5000),
    foo(Port).




-ifdef(TEST).

%simple_test() ->
    
 %   ?MODULE:foo(3333).


init_test() ->
    
   {ok, {Policy,[{Id,StartFunc,Restart,Shutdown,Type,Modules}]}}= ?MODULE:init([4444]).

    
%startlink_test()->
%    ok=?MODULE:start_link(5555).

-endif.

