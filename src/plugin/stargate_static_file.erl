-module(stargate_static_file).
-compile(export_all).

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

serve_static(Path, Headers, S) ->
    Fullpath = <<(sanitize_path(Path))/binary>>,
    case filelib:is_regular(Fullpath) of
        false -> 
            {404, #{}, <<>>, S};

        true ->
            {ok, Bin} = file:read_file(Fullpath),
            GZipBin = zlib:gzip(Bin),

            Etag = maps:get('If-None-Match', Headers, undefined),
            Crc32 = erlang:integer_to_binary(erlang:crc32(GZipBin)),
            if
                Etag == Crc32 -> 
                    {304, #{}, <<>>, S};
                true -> 
                    {200, #{<<"Content-Encoding">>=> <<"gzip">>, <<"Etag">>=> Crc32}, GZipBin, S}
            end
    end
    .