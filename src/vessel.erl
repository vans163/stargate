-module(vessel).
-behaviour(gen_server).

-export([start/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("global.hrl").

start(Params) -> {ok, Pid} = start_link(Params), Pid.
start_link(Params) -> gen_server:start(?MODULE, Params, []).

init({Paths, Socket}) ->
    process_flag(trap_exit, true),
    timer:send_after(?TIMEOUT, stalled_connection),

    {ok, #{paths=> Paths, session_state=> #{}}}.

handle_cast({pass_socket, ClientSocket}, S) ->
    inet:setopts(ClientSocket, [{active, once}, {packet, http_bin}]),
    {noreply, S#{socket=> ClientSocket}};

handle_cast(Message, S) -> {noreply, S}.
handle_call(Message, From, S) -> {reply, ok, S}.

handle_info({http, Socket, {http_request, Type, {abs_path, Path}, HttpVer}}, S=#{
    session_state:= SessState, paths:=Paths
}) ->
    %Type = 'GET'/'POST'/'ETC'
    {HttpHeaders, Body} = proto_http:recv(Socket),

    Host = maps:get('Host', HttpHeaders, <<"*">>),
    WildCardAtom = maps:get(<<"*">>, Paths),
    HandlerAtom = maps:get(Host, Paths, WildCardAtom),

    {CleanPath, Query} = proto_http:path_and_query(Path),

    {ResponseCode, ResponseHeaders, ResponseBody, SessState2} = 
        apply(HandlerAtom, http, 
            [Type, CleanPath, Query, HttpHeaders, Body, 
                SessState#{socket=> Socket, host=> Host}
            ]
    ),

    ResponseBin = proto_http:response(ResponseCode, ResponseHeaders, ResponseBody),
    ok = gen_tcp:send(Socket, ResponseBin),
    gen_tcp:close(Socket),

    {noreply, S#{socket=> Socket, session_state=> SessState2}}
;

handle_info({tcp_closed, Socket}, S) ->
    {stop, {shutdown, tcp_closed}, S};

handle_info(stalled_connection, S) ->
    case maps:get(socket, S, undefined) of
        undefined -> pass;
        Socket -> gen_tcp:close(Socket)
    end,
    {stop, {shutdown, tcp_closed}, S};

handle_info(Message, S) ->
    ?PRINT({"INFO", Message, S}),
    {noreply, S}.


terminate({shutdown, tcp_closed}, S) -> ok;
terminate(_Reason, S) -> 
    ?PRINT({"Terminated", _Reason, S})
    .

code_change(_OldVersion, S, _Extra) -> {ok, S}. 
