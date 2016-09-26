-module(stargate_child).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("../global.hrl").


start_link(CleanArgs) -> gen_server:start_link(?MODULE, CleanArgs, []).

init(CleanArgs=#{ip:=BindIp, port:=BindPort}) ->

    {ok, ListenSocket} = gen_tcp:listen(Port, [
        {ip, Ip}, {active, false}, {reuseaddr, true}
    ]),

    APid = handle_restart_acceptors(CleanArgs, ListenSocket, undefined),

    {ok, #{
        acceptor_sup=> APid,
        listen_socket=> ListenSocket, 
        params=> CleanArgs
        }
    }.

handle_restart_acceptors(CleanArgs, ListenSocket, undefined) ->
    {ok, APid} = stargate_acceptor_sup:start_link(CleanArgs, ListenSocket),
    APid;
handle_restart_acceptors(CleanArgs, ListenSocket, AcceptorSup) ->
    exit(AcceptorSup, shutdown),
    {ok, APid} = stargate_acceptor_sup:start_link(CleanArgs, ListenSocket),
    APid;


handle_info({update_params, NewParams}, 
    S=#{
        listen_socket:= ListenSocket, 
        acceptor_sup:= ASupPid
        params:= P=#{hosts:= Hosts, ssl_opts:= SSLOpts}
    }
)->
    NewHosts = maps:get(hosts, NewParams, #{}),
    NewSSLOpts = maps:get(ssl_opts, NewParams, []),

    MergedHosts = maps:merge(Hosts, NewHosts),
    MergedSSLOpts = lists:ukeysort(1, NewSSLOpts ++ SSLOpts),

    P2 = P#{hosts=> MergedHosts, ssl_opts=> MergedSSLOpts},
    APid = handle_restart_acceptors(P2, ListenSocket, ASupPid),

    {noreply, S2#{acceptor_sup=> APid, params=> P2}};

handle_info(_Message, S) -> {noreply, S}.

terminate(_Reason, S) -> ?PRINT({"stargate_child terminated:", _Reason, S}).


code_change(_OldVersion, S, _Extra) -> {ok, S}. 
handle_cast(Message, S) -> {noreply, S}.
handle_call(Message, From, S) -> {reply, ok, S}.