-module(stargate_plugin).
-compile(export_all).

serve_static(Base, Path, Headers, S) -> stargate_static_file:serve_static(Base, Path, Headers, S).
serve_static_bin(Bin, Headers, S) -> stargate_static_file:serve_static_bin(Bin, Headers, S).

template(Page, KeyValue) -> stargate_template:transform(Page, KeyValue).
template(RE, Page, KeyValue) -> stargate_template:transform(RE, Page, KeyValue).

%{text, Payload}
%{bin, Payload}
%{text_compress, Payload}
%{bin_compress, Payload}
ws_send(VesselPid, {Type, Payload}) -> VesselPid ! {ws_send, {Type, Payload}}.
ws_message(VesselPid, Msg) -> VesselPid ! {ws_message, Msg}.

%TODO: Does this conform/work in all cases?
cookie_parse(CookiesBin) -> 
    CookiesSplit = binary:split(CookiesBin, <<"; ">>, [global]),
    lists:foldl(fun(CookieBin, Acc) ->
            case binary:split(CookieBin, <<"=">>) of
                [K, V] -> Acc#{K=> V};
                _ -> Acc
            end
        end, #{}, CookiesSplit
    ).


