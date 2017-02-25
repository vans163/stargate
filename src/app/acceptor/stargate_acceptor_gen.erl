-module(stargate_acceptor_gen).
-behaviour(gen_server).
-compile(export_all).

%-import(stargate_vessel, [start/1]).

handle_cast(_Message, S) -> {noreply, S}.
handle_call(_Message, _From, S) -> {reply, ok, S}.
code_change(_OldVersion, S, _Extra) -> {ok, S}. 

start_link(Params) -> gen_server:start(?MODULE, Params, []).

init({StargateArgs, ListenSocket, Id}) ->
    {ok, _} = prim_inet:async_accept(ListenSocket, -1),
    {ok, #{params=> StargateArgs, listen_socket=> ListenSocket, id=> Id}}.

terminate(Reason, S) -> 
    io:format("~p:~n Terminated~n ~p~n ~p~n", [?MODULE, Reason, S]).



handle_info({inet_async, ListenSocket, _, {ok, ClientSocket}}, S=#{params:= Params}) ->
    prim_inet:async_accept(ListenSocket, -1),
    
    {ok, Pid} = stargate_vessel:start(Params),

    inet_db:register_socket(ClientSocket, inet_tcp),
    ok = gen_tcp:controlling_process(ClientSocket, Pid),

    Pid ! {pass_socket, ClientSocket},

    {noreply, S};

%TODO: Dont shutdown on all errors, see what is critical or not
handle_info({inet_async, _ListenSocket, _, Error}, S) ->
    io:format("~p:~n Error in inet_async accept, shutting down~n ~p~n", 
        [?MODULE, Error]),
    {stop, Error, S};

handle_info(Message, S) -> 
    io:format("~p:~n Unhandled~n ~p~n ~p~n", [?MODULE, Message, S]),
    {noreply, S}.

