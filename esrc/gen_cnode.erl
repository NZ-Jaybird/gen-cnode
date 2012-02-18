-module( gen_cnode ).
-behaviour(gen_server).

%state of gen_cnode
-record( gen_cnode_state, {name, host, port, hostname, libs, workers, cnode, event_manager} ).

%%gen_server callbacks
-export( [  start_link/1,
            init/1,
            handle_call/3,
            handle_cast/2,
            handle_info/2,
            code_change/3,
            terminate/2   
         ] ).

%% Defines state defaults based on whether FQDN are being used
defaults( true ) ->

    %%Just use IPv4 as all communication is over localhost
    {ok, {A,B,C,D}} = inet:ip("localhost"),

    Name = "c0",

    %%Cheap hack..stitch together IP string
    Host = integer_to_list(A) ++ "." ++ 
           integer_to_list(B) ++ "." ++
           integer_to_list(C) ++ "." ++
           integer_to_list(D),

    Cnode = Name ++ "@" ++ Host,
   
    %%<<HERE>> resolve localhost to full IP
    #gen_cnode_state{ name=list_to_atom(Name), 
                      host=list_to_atom(Host),
                      port=30000, 
                      workers=0,
                      libs=[], 
                      cnode=list_to_atom(Cnode),
                      event_manager = nil };

defaults( false ) ->
    #gen_cnode_state{ name='c0', 
                      host='localhost',
                      port=30000, 
                      workers=0,
                      libs=[], 
                      cnode='c0_cnode@localhost',
                      event_manager = nil }.

%% Individual argument parsing
parse_arg( {workers, Workers}, State ) when is_integer(Workers) ->
    State#gen_cnode_state{ workers = Workers };

parse_arg( {libs, Libs}, State ) when is_list(Libs) ->
    State#gen_cnode_state{ libs = Libs };

parse_arg( {name, Name}, State ) when is_atom(Name) ->
    Node = list_to_atom( atom_to_list(Name) ++ 
                         "_cnode@" ++ 
                         atom_to_list(State#gen_cnode_state.host) ),

    io:format("Name: ~s, Node: ~s~n", [Name, Node]),

    State#gen_cnode_state{ name=Name, cnode=Node };

parse_arg( {port, Port}, State ) when is_integer(Port) ->
    State#gen_cnode_state{ port=Port };

parse_arg( {event_manager, Manager}, State ) when is_atom(Manager) ->
    State#gen_cnode_state{ event_manager=Manager };

parse_arg( Unknown, State ) when is_tuple( Unknown ) -> State.

%% Recurse over given args and build up initial state
parse_args( [], State ) -> State;
parse_args( [H | T], State ) when is_tuple(H) ->
    parse_args(T, parse_arg( H, State )).

start_link( Args ) when is_list(Args) ->

    %%Initialize state by merging defaults and options
    State = parse_args( Args, defaults(net_kernel:longnames()) ),
   
    gen_server:start_link({local, State#gen_cnode_state.name}, ?MODULE, State, []).

%%Build up gen_cnode command based on state
exec_gen_cnode( State ) when is_record( State, gen_cnode_state ) ->
    Command = io_lib:format( code:priv_dir(?MODULE) ++ "/gen_cnode -n ~s -h ~s -p ~w -w ~w -s ~s", 
                                [   State#gen_cnode_state.name,
                                    State#gen_cnode_state.host,
                                    State#gen_cnode_state.port,
                                    State#gen_cnode_state.workers, 
                                    erlang:get_cookie() ] ),  
    error_logger:info_msg("Calling: ~s~n", [ Command ]),
    os:cmd( Command ).
    %%error_logger:info_msg("C process exited."
    %%                      "~n------Output: Start------~n"
    %%                      "~s"
    %%                      "~n------Output: End------~n", [Out]).   

handshake( Max, Max, _Timeout, _State ) -> 
    { stop, "Failed to connect to C process!" };
handshake( Tries, Max, Timeout, State ) ->
    
    Tag = erlang:make_ref(),

    %%Handshake, block until C side is fully functional
    {any, State#gen_cnode_state.cnode} ! { parent, {self(), Tag}, {gen_cnode, ping, []} },
   
    %% Wait for response 
    receive
        {Tag, {pong, _Time}} -> 
            { ok, State };

        {Tag, Something} ->
            io:format("Got: ~p~n", [Something])

        after Timeout ->
            handshake( Tries + 1, Max, Timeout, State )
    end.

%% Parse args and exec gen_cnode binary
init( State ) when is_record(State, gen_cnode_state) ->

    %%Shutdown gen_cnode binary on exit
    process_flag( trap_exit, true ),

    %%Start gen_cnode binary and link to it
    spawn_link( fun() -> exec_gen_cnode( State ) end ),

    handshake( 0, 10, 100, State ).

%%%%%%%%%%   GEN_SEVER CALLS %%%%%%%%%%%%%%%

%% Signal gen_cnode process to load the specified library

handle_call( ping, _From, State ) ->
    { reply, pong, State };

handle_call( {load, Lib}, _From, State ) when is_atom( Lib ) ->
    {any, State#gen_cnode_state.cnode} ! {parent, _From, {gen_cnode, load, [Lib]}},
    {noreply, State};

handle_call( {load, Libs}, _From, State ) when is_list( Libs ) ->
    {any, State#gen_cnode_state.cnode} ! { parent, _From, {gen_cnode, load, Libs} },
    {noreply, State};

%% Signal gen_cnode to perfrom the specified routine
handle_call( {Lib, Func}, _From, State ) when is_atom(Lib) and
                                              is_atom(Func) ->
    {any, State#gen_cnode_state.cnode} ! { parent, _From, {Lib, Func, []} },
    {noreply, State};

handle_call( {Actor, Lib, Func}, _From, State ) when is_atom(Actor) and
                                                     is_atom(Lib) and 
                                                     is_atom(Func) ->
    {any, State#gen_cnode_state.cnode} ! { Actor, _From, {Lib, Func, []} },
    {noreply, State};

handle_call( {Lib, Func, Args}, _From, State ) when is_atom(Lib)  and 
                                                    is_atom(Func) and 
                                                    is_list(Args) ->

    {any, State#gen_cnode_state.cnode} ! { parent, _From, {Lib, Func, Args} },
    {noreply, State};

handle_call( {Actor, Lib, Func, Args}, _From, State ) when is_atom(Actor) and
                                                           is_atom(Lib)  and 
                                                           is_atom(Func) and 
                                                           is_list(Args) ->

    {any, State#gen_cnode_state.cnode} ! { Actor, _From, {Lib, Func, Args} },
    {noreply, State};

handle_call( _Request, _From, State ) -> 
    io:format("Got call...not sure what to do with it...~n"),
    {noreply, State}.


%%%%%%%%%%   GEN_SEVER CASTS %%%%%%%%%%%%%%%
%% Event dispatcher for the C side
handle_cast( {event, Msg}, State ) when State#gen_cnode_state.event_manager /= nil ->
    gen_event:notify(State#gen_cnode_state.event_manager, Msg),
    {noreply, State}; 

handle_cast( {event, _Msg}, State ) ->
   {noreply, State}; 

handle_cast( {Lib, Func}, State ) when is_atom(Lib) and
                                       is_atom(Func) ->
    {any, State#gen_cnode_state.cnode} ! { parent, {Lib, Func, []} },
    {noreply, State};

handle_cast( {Actor, Lib, Func}, State ) when is_atom(Actor) and
                                                     is_atom(Lib) and 
                                                     is_atom(Func) ->
    {any, State#gen_cnode_state.cnode} ! { Actor, {Lib, Func, []} },
    {noreply, State};

handle_cast( {Lib, Func, Args}, State ) when is_atom(Lib)  and 
                                             is_atom(Func) and 
                                             is_list(Args) ->

    {any, State#gen_cnode_state.cnode} ! { parent, {Lib, Func, Args} },
    {noreply, State};

handle_cast( {Actor, Lib, Func, Args}, State ) when is_atom(Actor) and
                                                    is_atom(Lib)  and 
                                                    is_atom(Func) and 
                                                    is_list(Args) ->

    {any, State#gen_cnode_state.cnode} ! { Actor, {Lib, Func, Args} },
    {noreply, State};

handle_cast( stop, State ) -> {stop, normal, State};

handle_cast( _Request, State ) -> 
    io:format("Got cast...not sure what to do with it...~n"),
    {noreply, State}.

%%Handle trap_exit on child C gen_cnode
handle_info( {'EXIT', _Pid, _Reason}, State ) ->
    error_logger:error_info("C process went away unexpectedly! Exiting!"),    
    %%TODO: Handle output
    {stop, normal, State};

handle_info( _Info, State) -> {noreply, State}.

terminate(_Reason, State) ->
    %% Signal C gen_cnode process to exit
    {any, State#gen_cnode_state.cnode} ! { parent, {gen_cnode, stop, []} }.

code_change(_OldVsn, State, _Extra ) -> {ok, State}.
