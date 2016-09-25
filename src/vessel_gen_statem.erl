-module(vessel_gen_statem).
-behaviour(gen_statem).

-compile(export_all).

-include("global.hrl").

%callback_mode() ->  state_functions.
callback_mode() ->  handle_event_function.

start_link(Params) -> gen_statem:start(?MODULE, Params, []).

init({Params, Socket}) ->
    process_flag(trap_exit, true),

    {ok, 
        waiting_socket,
        #{params=> Params, session_state=> #{}},
        ?MAX_TCP_TIMEOUT
        %{next_event, internal, init}
    }.

%Regular HTTP request
handle_http(Headers, Body, S=#{
    socket:=Socket, params:= #{hosts:=Hosts}, session_state:= SS=#{path:=Path, type:=Type}
}) ->
    Host = maps:get('Host', Headers, <<"*">>),
    WCardAtom = maps:get({http, <<"*">>}, Hosts),
    {HandlerAtom, HandlerOpts} = maps:get({http, Host}, Hosts, WCardAtom),

    {CleanPath, Query} = proto_http:path_and_query(Path),

    {RCode, RHeaders, RBody, SS2} = 
        apply(HandlerAtom, http, 
            [Type, CleanPath, Query, Headers, Body, 
                SS#{socket=> Socket, host=> Host}
            ]
    ),

    ConnectionHeader = case maps:get('Connection', Headers, <<"close">>) of
        <<"keep-alive">> -> <<"keep-alive">>;
        _ -> <<"close">>
    end,

    RBin = proto_http:response(RCode, RHeaders#{<<"Connection">>=> ConnectionHeader}, RBody),
    ok = transport_send(Socket, RBin),

    case ConnectionHeader of
        <<"keep-alive">> -> 
            ok = transport_setopts(Socket, [{active, once}, {packet, http_bin}]);
            
        _ ->
            %?PRINT({"closing transport"}), 
            ok = transport_close(Socket)
    end,
    S#{session_state=> SS2}
    .

%waiting_socket - SSL
handle_event(info, {pass_socket, ClientSocket}, waiting_socket, 
    D=#{params:= #{ssl_opts:= SSLOpts, error_logger:= ELogger}}) 
->
    case ssl:ssl_accept(ClientSocket, SSLOpts, 30000) of
        {ok, SSLSocket} ->
            ok = transport_setopts(SSLSocket, [{active, once}, {packet, http_bin}]),
            {next_state, https, D#{socket=> SSLSocket}};

        {error, Error} ->
            ELogger(ClientSocket, <<"ssl_accept">>, Error),
            {stop, {shutdown, tcp_closed}, D}
    end;
%waiting_socket - Regular
handle_event(info, {pass_socket, ClientSocket}, waiting_socket, D) ->
    ok = transport_setopts(ClientSocket, [{active, once}, {packet, http_bin}]),
    {next_state, http, D#{socket=> ClientSocket}};


%http_request http, ssl
handle_event(info, {T, Socket, {http_request, Type, {abs_path, Path}, HttpVer}}, 
    S, D) when T == http; T == ssl 
->
    SessState = maps:get(session_state, D),

    %Type = 'GET'/'POST'/'ETC'
    {HttpHeaders, Body} = proto_http:recv(Socket),
    D2 = handle_http(HttpHeaders, Body, D#{
        session_state=> SessState#{path=> Path, type=> Type}
    }),
    
    {next_state, S, D2, ?MAX_TCP_TIMEOUT};

%out tcp conn timed out
handle_event(timeout, EvContent, S, D) ->
    {stop, {shutdown, tcp_closed}, D};


handle_event(info, Msg, S, D) ->
    %?PRINT({"Unhandled msg", Msg, S, D}),
    {next_state, S, D}.


code_change(V, S, D, E) -> {ok, S, D}.

terminate({shutdown, tcp_closed}, S, D) -> ignore;
terminate(R, S, D) ->
    ?PRINT({"Terminated", R, S, D}),
    ok.