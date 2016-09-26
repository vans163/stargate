-module(stargate).

-export([warp_in/1, warp_in/2]).
-export([warp_out/1, warp_out/2]).
-export([update_params/2]).

-export([launch_demo/0]).

-include("global.hrl").


warp_in(StargateArgs) ->
    SupPid = whereis(stargate_sup),
    warp_in(SupPid, StargateArgs).
warp_out(Pid) ->
    SupPid = whereis(stargate_sup),
    warp_out(SupPid, Pid).

warp_in(SupPid, StargateArgs) ->
    stargate_sup:start_child(SupPid, fix_params(StargateArgs)).
warp_out(SupPid, Pid) ->
    stargate_sup:delete_child(SupPid, Pid).

update_params(Pid, NewParams) ->
    Pid ! {update_params, NewParams}.




launch_demo() ->
    {ok, _} = application:ensure_all_started(stargate),

    HttpArgs = #{
        port=> 8000,
        ip=> {0,0,0,0},
        hosts=> #{
            {http, <<"*">>}=> {?HANDLER_WILDCARD, #{}},
            {ws, <<"*">>}=> {?HANDLER_WILDCARD_WS, #{}}
        }
    },

    %openssl req -x509 -newkey rsa:2048 -keyout key.pem -out cert.pem -days 100 -nodes -subj '/CN=localhost'
    WSCompress = #{window_bits=> 15, level=>1, mem_level=>8, strategy=>default},
    HttpsArgs = #{
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
    },

    {ok, HttpServerPid} = warp_in(HttpArgs),
    {ok, HttpsServerPid} = warp_in(HttpsArgs).



ensure_wildcard(Hosts) ->
    Hosts2 = ensure_wildcard({http, <<"*">>}, ?HANDLER_WILDCARD, Hosts),
    Hosts3 = ensure_wildcard({ws, <<"*">>}, ?HANDLER_WILDCARD_WS, Hosts2).
ensure_wildcard(Key, Handler, Hosts) ->
    case maps:get(Key, Hosts, undefined) of
        undefined -> maps:put(Key, {Handler, #{}}, Hosts);
        _ -> Hosts
    end.

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
                    io:format("~p~n", [{?TRANSPORT_PEERNAME(Socket), What, Error, Trace}])
                end,
                Params2
            );
        _ -> Params2
    end.