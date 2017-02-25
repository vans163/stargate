-module(stargate_child_gen).
-behaviour(gen_server).

-compile(export_all).

%-import(stargate_acceptor_sup, [start_link/1]).

code_change(_OldVersion, S, _Extra) -> {ok, S}. 
handle_cast(_Message, S) -> {noreply, S}.
handle_call(_Message, _From, S) -> {reply, ok, S}.

start_link(CleanArgs) -> gen_server:start_link(?MODULE, CleanArgs, []).


init(CleanArgs=#{ip:=BindIp, port:=BindPort}) ->
    ListenArgs = maps:get(listen_args, CleanArgs, []),

    {ok, ListenSocket} = gen_tcp:listen(BindPort, ListenArgs ++ [
        {ip, BindIp}, {active, false}, {reuseaddr, true}, {nodelay, true}
    ]),

    APid = handle_restart_acceptors(CleanArgs, ListenSocket, undefined),

    {ok, #{
        acceptor_sup=> APid,
        listen_socket=> ListenSocket, 
        params=> CleanArgs
        }
    }.

terminate(Reason, S) -> 
    io:format("~p:~n Unhandled~n ~p~n ~p~n", [?MODULE, Reason, S]).



handle_restart_acceptors(CleanArgs, ListenSocket, undefined) ->
    {ok, APid} = stargate_acceptor_sup:start_link({CleanArgs, ListenSocket}),
    APid;
handle_restart_acceptors(CleanArgs, ListenSocket, AcceptorSup) ->
    exit(AcceptorSup, shutdown),
    {ok, APid} = stargate_acceptor_sup:start_link({CleanArgs, ListenSocket}),
    APid.


handle_info({update_params, NewParams}, 
    S=#{
        listen_socket:= ListenSocket, 
        acceptor_sup:= ASupPid,
        params:= P=#{hosts:= Hosts, ssl_opts:= SSLOpts}
    }
)->
    NewHosts = maps:get(hosts, NewParams, #{}),
    NewSSLOpts = maps:get(ssl_opts, NewParams, []),

    MergedHosts = maps:merge(Hosts, NewHosts),
    MergedSSLOpts = lists:ukeysort(1, NewSSLOpts ++ SSLOpts),

    P2 = P#{hosts=> MergedHosts, ssl_opts=> MergedSSLOpts},
    APid = handle_restart_acceptors(P2, ListenSocket, ASupPid),

    {noreply, S#{acceptor_sup=> APid, params=> P2}};

handle_info(Message, S) ->
    io:format("~p:~n Unhandled~n ~p~n ~p~n", [?MODULE, Message, S]),
    {noreply, S}.




