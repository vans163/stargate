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
            port=> 8000,
            ip=> {0,0,0,0},
            ssl=> false,
            hosts=> #{
                {http, <<"*">>}=> {?HANDLER_WILDCARD, #{}},
                {ws, <<"*">>}=> {?HANDLER_WILDCARD_WS, #{}}
            }
        }
    ),
    WSCompress = #{window_bits=> 15, level=>1, mem_level=>8, strategy=>default},
    start_link(#{
            port=> 8443,
            ip=> {0,0,0,0},
            ssl=> true,
            certfile=> "./priv/cert.pem",
            keyfile=> "./priv/key.pem",
            hosts=> #{
                {http, <<"*">>}=> {?HANDLER_WILDCARD, #{}},
                {ws, <<"*">>}=> {?HANDLER_WILDCARD_WS, #{compress=>WSCompress}}
            }
        }
    ).
%openssl req -x509 -newkey rsa:2048 -keyout key.pem -out cert.pem -days 100 -nodes -subj '/CN=localhost'

ensure_wildcard(Hosts) ->
    Hosts2 = ensure_wildcard({http, <<"*">>}, ?HANDLER_WILDCARD, Hosts),
    Hosts3 = ensure_wildcard({ws, <<"*">>}, ?HANDLER_WILDCARD_WS, Hosts2)
    .
ensure_wildcard(Key, Handler, Hosts) ->
    case maps:get(Key, Hosts, undefined) of
        undefined -> maps:put(Key, {Handler, #{}}, Hosts);
        _ -> Hosts
    end
    .

fix_params(Params) ->
    Port = maps:get(port, Params),
    Ip = maps:get(ip, Params),
    Hosts = maps:get(hosts, Params),

    Hosts2 = ensure_wildcard(Hosts),
    Params2 = maps:put(hosts, Hosts2, Params),

    Ssl = maps:get(ssl, Params),
    case Ssl of
        true -> ssl:start();
        _ -> pass
    end,
    Params2.
%%%%%%


start_link(Params) -> gen_server:start_link(?MODULE, fix_params(Params), []).

init(Params=#{ip:=BindIp, port:=BindPort}) ->
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


terminate(_Reason, S) -> ?PRINT({"Stargate terminated:", _Reason, S}).
code_change(_OldVersion, S, _Extra) -> {ok, S}. 
