-module( gen_cnode ).
-behaviour(gen_server).

%state of gen_cnode
-record( gen_cnode_state, {id, port, hostname, libs, cnode} ).

%API functions

%%gen_server callbacks
-export( [  start_link/1,
            init/1,
            handle_call/3,
            handle_cast/2,
            handle_info/2,
            code_change/3,
            terminate/2   
         ] ).

start_link( Args ) when is_list(Args) ->
    gen_server:start_link({local, ?MODULE}, gen_cnode, Args, []).

parse_arg( {libs, Libs}, #gen_cnode_state{id =I, port=P, libs=_L, cnode=_C} ) when is_list( Libs ) ->
    #gen_cnode_state{ id=I, port=P, libs=Libs, cnode=_C };

parse_arg( {id, ID}, #gen_cnode_state{id =_I, port=P, libs=L} ) ->
    {ok, Hostname} = inet:gethostname(),
    Node = list_to_atom( lists:append( io_lib:format("c~p@", [ID] ), Hostname) ),
    #gen_cnode_state{ id=ID, port=P, libs=L, cnode=Node };

parse_arg( {port, Port}, #gen_cnode_state{id =I, port=_P, libs=L, cnode=_C} ) ->
    #gen_cnode_state{ id=I, port=Port, libs=L, cnode=_C };

parse_arg( Unknown, State ) when is_tuple( Unknown ) -> State.

%% Recurse over given args and build up initial state
parse_args( [], State ) -> State;
parse_args( [H | T], State ) when is_tuple(H) ->
    parse_args(T, parse_arg( H, State )).

%%Build up gen_cnode command based on state
exec_gen_cnode( State ) when is_record( State, gen_cnode_state ) ->
    Command = io_lib:format( "gen_cnode -I ~p -P ~p -S ~s", 
                                [   State#gen_cnode_state.id, 
                                    State#gen_cnode_state.port, 
                                    erlang:get_cookie() ] ),  
    error_logger:info_msg("Calling: ~s~n", [ Command ]),
    os:cmd( Command ).   %% <<HERE>> Load the Libs listed in initial state

%% Parse args and exec gen_cnode binary
init( Args ) ->
    {ok, Hostname} = inet:gethostname(),
    Node = list_to_atom( lists:append( "c0@", Hostname) ),
    State = parse_args( Args, #gen_cnode_state{ id=0, 
                                                port=40000, 
                                                hostname=Hostname, 
                                                libs=[], 
                                                cnode=Node } ),
    
    %%Start gen_cnode binary
    spawn( fun() -> exec_gen_cnode( State ) end ),
   
    %%Shutdown gen_cnode binary an exit
    process_flag( trap_exit, true ),

    { ok, State }.

%% Signal gen_cnode process to load the specified library
handle_call( {load, Libs}, _From, State ) when is_list( Libs ) ->
    {any, State#gen_cnode_state.cnode} ! { call, self(), {gen_cnode, load, Libs} },

    receive
        Reply ->
            {reply, Reply, State}
    end;

%% Signal gen_cnode to perfrom the specified routine
handle_call( {Lib, Func, Args}, _From, State ) when is_atom(Lib)  and 
                                                    is_atom(Func) and 
                                                    is_list(Args) ->
    
    {any, State#gen_cnode_state.cnode} ! { call, self(), {Lib, Func, Args} },

    receive
        Reply ->
            {reply, Reply, State}
    end;

handle_call( _Request, _From, State ) -> {noreply, State}.

handle_cast( stop, State ) -> terminate( normal, State ), {noreply, State};
handle_cast( _Request, State ) -> {noreply, State}.
handle_info( _Info, State) -> {noreply, State}.

%% Signal gen_cnode process to halt
terminate(_Reason, State) -> 
    {any, State#gen_cnode_state.cnode} ! { call, self(), {gen_cnode, stop, []} }.

code_change(_OldVsn, State, _Extra ) -> {ok, State}.
