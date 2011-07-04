-module( unit_tests ).

-import( gen_cnode ).

-export( [ start/0,
           callrate/1,
           callrate_erl/1 ] ).

%Start gen_cnode process and see how many function calls we can manage
start( ) ->
    
    {ok, _Pid} = gen_cnode:start_link( [{name, test}, {port, 30000}, {workers, -1}] ),

    %%load the test library
    ok = gen_server:call( test, {load, test} ).


%Generates a certain number of gen_cnode pings
ping_cnode( Max, Max ) -> ok;
ping_cnode( Call, Max ) when (Call < Max) ->
    pong = gen_server:call( test, {gen_cnode, ping} ),
    ping_cnode( Call + 1, Max ).

%Tests turnaround time from erlang to cnode and back again.
callrate( Calls ) ->
    Begin = erlang:time(),
    ping_cnode(0, Calls),
    End = erlang:time(),
    io:format( "Begin: ~p, End: ~p~n", [Begin, End] ).

%Erlang gen_server ping
ping( Max, Max ) -> ok;
ping( Call, Max ) when (Call < Max) ->
    pong = gen_server:call( test, ping ),
    ping( Call + 1, Max ).

%Tests turnaround time from erlang to cnode and back again (baseline)
callrate_erl( Calls ) ->
    Begin = erlang:time(),
    ping( 0, Calls ), 
    End = erlang:time(),
    io:format( "Begin: ~p, End: ~p~n", [Begin, End] ).
