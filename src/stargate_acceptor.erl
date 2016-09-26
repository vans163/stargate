-module(stargate_acceptor).
-behaviour(gen_server).

-compile(export_all).

-include("global.hrl").

start_link(Params) -> gen_server:start(?MODULE, Params, []).

init({StargateArgs, ListenSocket, Id}) ->
    process_flag(trap_exit, true),
    {ok, _} = prim_inet:async_accept(ListenSocket, -1),
    {ok, #{params=> StargateArgs, listen_socket=> ListenSocket, id=> Id}}.


handle_info({inet_async, ListenSocket, _, {ok, ClientSocket}}, S=#{params:= Params}) ->
    prim_inet:async_accept(ListenSocket, -1),
    
    {ok, Pid} = vessel:start(Params),

    inet_db:register_socket(ClientSocket, inet_tcp),
    ok = gen_tcp:controlling_process(ClientSocket, Pid),

    Pid ! {pass_socket, ClientSocket},

    {noreply, S};

%TODO: Dont shutdown on all errors, see what is critical or not
handle_info({inet_async, _ListenSocket, _, Error}, S) ->
    ?PRINT({"Error in inet_async accept, shutting down", Error}),
    {stop, Error, S};

handle_info(_Message, S) -> {noreply, S}.

terminate(_Reason, S) -> 
    ?PRINT({"Terminated", _Reason, S}).


handle_cast(_Message, S) -> {noreply, S}.
handle_call(_Message, _From, S) -> {reply, ok, S}.
code_change(_OldVersion, S, _Extra) -> {ok, S}. 

