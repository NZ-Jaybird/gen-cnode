-module( call_rate ).

-import( gen_cnode ).

-export( [ start/0,
           run/2 ] ).

%Start gen_cnode process and see how many function calls we can manage
start( ) ->
    {ok, Pid} = gen_cnode:start_link( [] ),
    Pid. 

callback( _Pid, Max, Max ) -> ok;
callback( Pid, Call, Max ) when (Call < Max) ->
    gen_server:call( Pid, { hello_world, hello_world, [] } ),
    callback( Pid, (Call + 1), Max ).

run( Pid, Calls ) ->
    io:format("Ensuring library is loaded...~n"),
    gen_server:call( Pid, {load, [hello_world]} ),
    Begin = erlang:time(),
    callback( Pid, 0, Calls ),    
    End = erlang:time(),
    io:format( "Begin: ~p, End: ~p~n", [Begin, End] ).


