-module(stargate_static_file).
-compile(export_all).

-ifndef(PRINT).
-define(PRINT(Var), io:format("~p:~p - ~p~n", [?MODULE, ?LINE, Var])).
-endif.

sanitize_forward_slash(Path) ->
    PathCleaner = binary:replace(Path, <<"//">>, <<"/">>),
    case binary:match(PathCleaner, <<"//">>) of
        nomatch -> 
            case PathCleaner of
                <<"/", PathClean/binary>> -> PathClean;
                _ -> PathCleaner
            end;

        _ -> sanitize_forward_slash(PathCleaner)
    end
    .

sanitize_path(Path) ->
    _Dirname = filename:dirname(Path),
    Dirname = re:replace(_Dirname, "[^0-9A-Za-z\\-\\_\\/]", "", [global, {return, binary}]),

    _Filename = filename:basename(Path),
    Filename = re:replace(_Filename, "[^0-9A-Za-z\\-\\_\\.]", "", [global, {return, binary}]),

    _Fullpath = <<Dirname/binary, "/", Filename/binary>>,
    sanitize_forward_slash(_Fullpath)
.

can_accept_gzip(Headers) ->
    case maps:get('Accept-Encoding', Headers, undefined) of
        undefined -> no;
        Bin ->
            case binary:match(Bin, <<"gzip">>) of
                nomatch -> no;
                _ -> yes
            end
    end
    .

serve_static(Base, DirtyPath, Headers, S) ->
    Fullpath = <<Base/binary, (sanitize_path(DirtyPath))/binary>>,
    case filelib:is_regular(Fullpath) of
        false -> 
            {404, #{}, <<>>, S};

        true ->
            {ok, Bin} = file:read_file(Fullpath),

            Etag = maps:get('If-None-Match', Headers, undefined),

            CanGZip = can_accept_gzip(Headers),
            {Code, HeadersReply, BinReply} = case filename:extension(Fullpath) of
                <<".png">> -> serve_static_no_gzip(Bin, Etag);
                <<".jpg">> -> serve_static_no_gzip(Bin, Etag);
                _ when CanGZip == yes -> serve_static_gzip(Bin, Etag);
                _ -> serve_static_no_gzip(Bin, Etag)
            end,

            {Code, HeadersReply, BinReply, S}
    end
    .

serve_static_no_gzip(Bin, Etag) ->
    Crc32 = erlang:integer_to_binary(erlang:crc32(Bin)),
    if
        Etag == Crc32 -> 
            {304, #{}, <<>>};
        true -> 
            {200, #{<<"Etag">>=> Crc32}, Bin}
    end.

serve_static_gzip(Bin, Etag) ->
    GZipBin = zlib:gzip(Bin),
    Crc32 = erlang:integer_to_binary(erlang:crc32(GZipBin)),
    if
        Etag == Crc32 -> 
            {304, #{}, <<>>};
        true -> 
            {200, #{<<"Content-Encoding">>=> <<"gzip">>, <<"Etag">>=> Crc32}, GZipBin}
    end.



