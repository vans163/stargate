-module(stargate).
-behaviour(gen_server).

-export([warp_in/0]).
-export([start_link/1]).
-export([update_params/2]).

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
            ssl_opts=> [
                {certfile, "./priv/cert.pem"},
                {keyfile, "./priv/key.pem"}
            ],
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

    Ssl = maps:get(ssl_opts, Params, false),
    case Ssl of
        false -> pass;
        _ -> ssl:start()
    end,

    ErrorLogger = maps:get(error_logger, Params2, undefined),
    case ErrorLogger of
        undefined ->
            maps:put(error_logger, 
                fun(Socket, What, Error) -> 
                    Trace = try throw(42) catch 42 -> erlang:get_stacktrace() end,
                    io:format("~p~n", [{transport_peername(Socket), What, Error, Trace}])
                end,
                Params2
            );
        _ -> Params2
    end.
%%%%%%

update_params(Pid, NewParams) -> gen_server:cast(Pid, {update_params, NewParams}).


start_link(Params) -> gen_server:start_link(?MODULE, fix_params(Params), []).

init(Params=#{ip:=BindIp, port:=BindPort}) ->
    listen(BindIp, BindPort),
    {ok, #{params=> Params}}.
    

listen(Ip, Port) ->
    {ok, ListenSock} = gen_tcp:listen(Port, [{ip, Ip}, {active, false}, {reuseaddr, true}]),
    {ok, _} = prim_inet:async_accept(ListenSock, -1),
    ListenSock.


handle_call(Message, From, S) -> {reply, ok, S}.

handle_cast({update_params, NewParams}, S=#{params:= P=#{hosts:= Hosts, ssl_opts:= SSLOpts}}) ->
    NewHosts = maps:get(hosts, NewParams, #{}),
    NewSSLOpts = maps:get(ssl_opts, NewParams, []),

    MergedHosts = maps:merge(Hosts, NewHosts),
    MergedSSLOpts = lists:ukeysort(1, NewSSLOpts ++ SSLOpts),

    {noreply, S#{params=> P#{hosts=> MergedHosts, ssl_opts=> MergedSSLOpts}}};
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
