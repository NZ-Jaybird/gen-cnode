-module( gen_cnode ).
-behaviour(gen_server).

%state of gen_cnode
-record( gen_cnode_state, {name, port, hostname, libs, workers, cnode} ).

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
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

parse_arg( {workers, Workers}, State ) when is_integer(Workers) ->
    State#gen_cnode_state{ workers = Workers };

parse_arg( {libs, Libs}, State ) when is_list(Libs) ->
    State#gen_cnode_state{ libs = Libs };

parse_arg( {name, Name}, State ) when is_atom(Name) ->
    Node = list_to_atom( atom_to_list(Name) ++ "@localhost" ),
    State#gen_cnode_state{ name=Name, cnode=Node };

parse_arg( {port, Port}, State ) when is_integer(Port) ->
    State#gen_cnode_state{ port=Port };

parse_arg( Unknown, State ) when is_tuple( Unknown ) -> State.

%% Recurse over given args and build up initial state
parse_args( [], State ) -> State;
parse_args( [H | T], State ) when is_tuple(H) ->
    parse_args(T, parse_arg( H, State )).

%%Build up gen_cnode command based on state
exec_gen_cnode( State ) when is_record( State, gen_cnode_state ) ->
    Command = io_lib:format( code:priv_dir(?MODULE) ++ "/gen_cnode -n ~s -p ~w -w ~w -s ~s", 
                                [   State#gen_cnode_state.name, 
                                    State#gen_cnode_state.port,
                                    State#gen_cnode_state.workers, 
                                    erlang:get_cookie() ] ),  
    error_logger:info_msg("Calling: ~s~n", [ Command ]),
    Out = os:cmd( Command ),
    error_logger:info_msg("C process exited."
                          "~n------Output: Start------~n"
                          "~s"
                          "~n------Output: End------~n", [Out]).   

%% Parse args and exec gen_cnode binary
init( Args ) ->
    State = parse_args( Args, #gen_cnode_state{ name='c0', 
                                                port=30000, 
                                                workers=0,
                                                libs=[], 
                                                cnode='c0@localhost' } ),
    
    %%Start gen_cnode binary and link to it
    spawn_link( fun() -> exec_gen_cnode( State ) end ),

    %%Shutdown gen_cnode binary on exit
    process_flag( trap_exit, true ),

    %%register ourselves under State.name
    unregister(?MODULE),
    true = register(State#gen_cnode_state.name, self()),

    { ok, State }.

%% Signal gen_cnode process to load the specified library
handle_call( {load, Lib}, _From, State ) when is_atom( Lib ) ->
    {any, State#gen_cnode_state.cnode} ! {parent, self(), {gen_cnode, load, [Lib]}},

    receive
        Reply ->
            {reply, Reply, State}
    end;

handle_call( {load, Libs}, _From, State ) when is_list( Libs ) ->
    {any, State#gen_cnode_state.cnode} ! { parent, self(), {gen_cnode, load, Libs} },

    receive
        Reply ->
            {reply, Reply, State}
    end;

%% Signal gen_cnode to perfrom the specified routine
handle_call( {Lib, Func}, _From, State ) when is_atom(Lib) and
                                              is_atom(Func) ->
    {any, State#gen_cnode_state.cnode} ! { parent, self(), {Lib, Func, []} },

    receive
        Reply ->
            {reply, Reply, State}
    end;

handle_call( {Actor, Lib, Func}, _From, State ) when is_atom(Actor) and
                                                     is_atom(Lib) and 
                                                     is_atom(Func) ->
    {any, State#gen_cnode_state.cnode} ! { Actor, self(), {Lib, Func, []} },

    receive
        Reply ->
            {reply, Reply, State}
    end;

handle_call( {Lib, Func, Args}, _From, State ) when is_atom(Lib)  and 
                                                    is_atom(Func) and 
                                                    is_list(Args) ->

    {any, State#gen_cnode_state.cnode} ! { parent, self(), {Lib, Func, Args} },

    receive
        Reply ->
            {reply, Reply, State}
    end;

handle_call( {Actor, Lib, Func, Args}, _From, State ) when is_atom(Actor) and
                                                           is_atom(Lib)  and 
                                                           is_atom(Func) and 
                                                           is_list(Args) ->

    {any, State#gen_cnode_state.cnode} ! { Actor, self(), {Lib, Func, Args} },

    receive
        Reply ->
            {reply, Reply, State}
    end;

handle_call( _Request, _From, State ) -> {noreply, State}.

handle_cast( stop, State ) -> {stop, normal, State};
handle_cast( _Request, State ) -> {noreply, State}.

%%Handle trap_exit on child C gen_cnode
handle_info( {'EXIT', _Pid, _Reason}, State ) ->
    error_logger:error_info("C process went away unexpectedly! Exiting!"),    
    {stop, normal, State};

handle_info( _Info, State) -> {noreply, State}.

terminate(_Reason, State) ->
    %% Signal C gen_cnode process to exit
    {any, State#gen_cnode_state.cnode} ! { parent, self(), {gen_cnode, stop, []} }.

code_change(_OldVsn, State, _Extra ) -> {ok, State}.
