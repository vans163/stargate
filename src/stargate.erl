-module(stargate).
-compile(export_all).

%-import([stargate_sup, [start_child/1]).


warp_in(StargateArgs) ->
    {ok, _Pid} = stargate_sup:start_child(fix_params(StargateArgs)).
warp_out(Pid) ->
    stargate_sup:terminate_child(Pid).

update_params(Pid, NewParams) ->
    Pid ! {update_params, NewParams}.




launch_demo() ->
    {ok, _} = application:ensure_all_started(stargate),

    HttpArgs = #{
        port=> 8000,
        ip=> {0,0,0,0},
        listen_args=> [],
        hosts=> #{
            {http, "*"}=> {stargate_handler_wildcard, #{}},
            {ws, "*"}=> {stargate_handler_wildcard_ws, #{}}
        }
    },

    %openssl req -x509 -newkey rsa:2048 -keyout key.pem -out cert.pem -days 100 -nodes -subj '/CN=localhost'
    WSCompress = #{window_bits=> 15, level=>1, mem_level=>8, strategy=>default},
    HttpsArgs = #{
        port=> 8443,
        ip=> {0,0,0,0},
        listen_args=> [],
        ssl_opts=> [
            {certfile, "./priv/cert.pem"},
            {keyfile, "./priv/key.pem"}
        ],
        hosts=> #{
            {http, "*"}=> {stargate_handler_wildcard, #{}},
            {ws, "*"}=> {stargate_handler_wildcard_ws, #{compress=>WSCompress}}
        }
    },

    {ok, HttpServerPid} = warp_in(HttpArgs),
    {ok, HttpsServerPid} = warp_in(HttpsArgs).



ensure_wildcard(Hosts) ->
    Hosts2 = ensure_wildcard({http, <<"*">>}, stargate_handler_wildcard, Hosts),
    Hosts3 = ensure_wildcard({ws, <<"*">>}, stargate_handler_wildcard_ws, Hosts2),
    Hosts3.
ensure_wildcard(Key, Handler, Hosts) ->
    case maps:get(Key, Hosts, undefined) of
        undefined -> maps:put(Key, {Handler, #{}}, Hosts);
        _ -> Hosts
    end.

host_strings_to_binary(Hosts) ->
    lists:foldr(fun(K, NewHosts) -> 
            V = maps:get(K, Hosts),
            K2 = case K of
                {T, {Bin1, Bin2}} ->
                    KBin1 = unicode:characters_to_binary(Bin1),
                    KBin2 = unicode:characters_to_binary(Bin2),
                    {T, {KBin1, KBin2}};

                {T, Bin} ->
                    KBin = unicode:characters_to_binary(Bin),
                    {T, KBin}
            end,
            maps:put(K2, V, NewHosts)
        end, 
        #{}, maps:keys(Hosts)
    ).

fix_params(Params) ->
    _Hosts = maps:get(hosts, Params),
    Hosts = host_strings_to_binary(_Hosts),

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
                    io:format("~p~n", [{Socket, What, Error, Trace}])
                end,
                Params2
            );
        _ -> Params2
    end.
