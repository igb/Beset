-module(beset_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.




%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, [Port]) ->
    io:fwrite("Starting...~p", [Port]),
    beset_sup:start_link(Port).

stop(_State) ->
    ok.




-ifdef(TEST).

simple_test() ->
  ok.  
%    ok = application:start(beset),
 %       ?assertNot(undefined == whereis(beset)).
-endif.
