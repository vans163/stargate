-module(stargate).
-behaviour(gen_server).

-export([warp_in/0]).
-export([start_link/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("global.hrl").

%%%%%%
warp_in() ->
    start_link(#{
            port=> 80,
            ip=> {0,0,0,0},
            paths=> #{<<"*">>=> ?HANDLER_WILDCARD}
        }
    ).

ensure_wildcard(Paths) ->
    case maps:get(<<"*">>, Paths, undefined) of
        undefined -> maps:put(<<"*">>, ?HANDLER_WILDCARD, Paths);
        _ -> Paths
    end.
%%%%%%


start_link(Params) ->
    Port = maps:get(port, Params),
    Ip = maps:get(ip, Params),
    Paths = maps:get(paths, Params),
    Paths2 = ensure_wildcard(Paths),

    gen_server:start_link(?MODULE, Params, []).

init(#{ip:=BindIp, port:=BindPort, params:=Params}) ->
    listen(BindIp, BindPort),
    {ok, #{params=> Params}}.
    

listen(Ip, Port) ->
    {ok, ListenSock} = gen_tcp:listen(Port, [{ip, Ip}, {active, false}, {reuseaddr, true}]),
    {ok, _} = prim_inet:async_accept(ListenSock, -1),
    ListenSock.


handle_call(Message, From, S) -> {reply, ok, S}.
handle_cast(Message, S) -> {noreply, S}.


handle_info({inet_async, ListenSocket, _, {ok, ClientSocket}}, S=#{params:= Params}) ->
    %register_socket or send will crash
    inet_db:register_socket(ClientSocket, inet_tcp),

    Pid = vessel:start({Params, ClientSocket}),
    gen_tcp:controlling_process(ClientSocket, Pid),
    gen_server:cast(Pid, {pass_socket, ClientSocket}),

    prim_inet:async_accept(ListenSocket, -1),
    {noreply, S};
handle_info(Message, S) -> {noreply, S}.


terminate(_Reason, S) ->
    ?PRINT({"WTF we died"}),
    io:format("~p:~p - ~p~n", [?MODULE, ?LINE, {"Terminated", _Reason, S}]).
code_change(_OldVersion, S, _Extra) -> {ok, S}. 