-module(acceptor).
-behaviour(gen_server).

-compile(export_all).

-include("global.hrl").

start_link(Params) -> gen_server:start(?MODULE, Params, []).

init({Params, ListenSocket, Id}) ->
    process_flag(trap_exit, true),
    %{ok, ListenSocket} = gen_tcp:listen(BindPort, [{ip, BindIp}, {active, false}, 
    %    {reuseaddr, true}]),


    {ok, _} = prim_inet:async_accept(ListenSocket, -1),
    {ok, #{params=> Params, listen_socket=> ListenSocket, id=> Id}}.


handle_info({inet_async, ListenSocket, _, {ok, ClientSocket}}, S=#{params:= Params}) ->
    %register_socket or send will crash
    %?PRINT({maps:get(id, S)}),
    inet_db:register_socket(ClientSocket, inet_tcp),

    {ok, Pid} = vessel_gen_statem:start_link({Params, ClientSocket}),
    ok = gen_tcp:controlling_process(ClientSocket, Pid),
    Pid ! {pass_socket, ClientSocket},

    %T1 = os:perf_counter(nanosecond),
    prim_inet:async_accept(ListenSocket, -1),
    %T2 = os:perf_counter(nanosecond),
    %?PRINT({T2 - T1}),
    {noreply, S};

handle_info({inet_async, ListenSocket, _, Error}, S) ->
    ?PRINT({"Error in inet_async accept, shutting down", Error}),
    {stop, Error, S};

handle_info(Message, S) ->
    ?PRINT({"INFO", Message}),
    {noreply, S}.

terminate(_Reason, S) -> 
    ?PRINT({"Terminated", _Reason, S}).


handle_cast(Message, S) -> {noreply, S}.
handle_call(Message, From, S) -> {reply, ok, S}.
code_change(_OldVersion, S, _Extra) -> {ok, S}. 

