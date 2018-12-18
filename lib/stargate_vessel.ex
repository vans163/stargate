defmodule Stargate.Vessel do
    @max_header_size 8192

    def loop(config) do
        config = receive do
            {:pass_socket, csocket} ->
                {transport, socket} = if !config[:ssl_opts] do
                    :ok = :inet.setopts(csocket, [{:active, true}])
                    {:gen_tcp, csocket}
                else
                    {:ok, ssl_socket} = :ssl.handshake(csocket, config.ssl_opts, 120000)
                    {:ssl, ssl_socket}
                end
                Map.merge(config, %{socket: socket, transport: transport})

            {:tcp, _socket, bin} -> on_tcp(config, bin)
            {:tcp_closed, _socket} -> on_close(config)
            {:tcp_error, _socket, _error} -> on_close(config)

            {:ssl, _socket, bin} -> on_tcp(config, bin)
            {:ssl_closed, _socket} -> on_close(config)
            {:ssl_error, _socket, _error} -> on_close(config)

            {:ws_send, tuple} -> on_ws_send(tuple, config)
        after
            120000 ->
                :close
        end
        loop(config)
    end

    def on_close(config) do
        config.transport.close(config.socket)
        Process.exit(self(),:normal)
    end

    def on_ws_send(_tuple, config) do
        config
    end

    def on_tcp(config=%{state: :ws}, bin) do
        buf = Map.get(config, :buf, <<>>) <> bin
        IO.inspect {:ws_bin, buf}
        config
    end
    def on_tcp(config=%{state: :http_body, body_size: bs}, bin) do
        buf = Map.get(config, :buf, <<>>) <> bin
        case buf do
            <<body::binary-size(bs), buf::binary>> ->
                request = Map.put(config.request, :body, body)
                config = proc_http_request(request, config)
                Map.merge(config, %{buf: buf, request: %{}, state: nil})

            buf -> 
                Map.merge(config, %{buf: buf})
        end
    end
    def on_tcp(config, bin) do
        buf = Map.get(config, :buf, <<>>) <> bin
        end_of_header? = :binary.match(buf, "\r\n\r\n")
        cond do
            byte_size(buf) > @max_header_size ->
                response_bin = build_http_response(413, [{"Connection", "close"}], "")
                :ok = config.transport.send(config.socket, response_bin)
                Process.exit(self(),:normal)

            end_of_header? == :nomatch -> Map.put(config, :buf, buf)

            is_tuple(end_of_header?) ->
                {pos,_} = end_of_header?
                <<header_bin::binary-size(pos), _::32, buf::binary>> = buf
                [req|headers] = String.split(header_bin, "\r\n")
                [type,path,http_ver] = String.split(req, " ")
                headers = Enum.map(headers, fn(line)->
                    [k,v] = String.split(line, ": ")
                    {String.downcase(k), v}
                end)

                #split out the query from the path
                {path, query} = case String.split(path, "?") do
                    [p,q] ->
                        kvmap = Enum.reduce(String.split(q, "&"),%{},fn(line,a)->
                            [k,v] = String.split(line, "=")
                            Map.put(a,k,v)
                        end)
                        {p, kvmap}
                    _ -> {path, %{}}
                end
                request = %{type: type, path: path, query: query, http_ver: http_ver, headers: headers, body: ""}

                websocket? = Enum.find(headers, fn({k,v})-> k == "upgrade" and v == "websocket" end)
                cond do
                   websocket? != nil -> 
                        config = proc_ws_handshake(request, config)
                        Map.merge(config, %{buf: buf, state: :ws})

                    type == "POST" or type == "PUT" ->
                        #add parsing more content types
                        {"content-length", clen} = Enum.find(headers, fn({k,_})-> k == "content-length" end)
                        clen = :erlang.binary_to_integer(clen)

                        case buf do
                            <<body::binary-size(clen), buf::binary>> ->
                                request = Map.put(request, :body, body)
                                config = proc_http_request(request, config)
                                Map.merge(config, %{buf: buf, request: %{}, state: nil})

                            buf ->
                                Map.merge(config, %{buf: buf, request: request, state: :http_body, body_size: clen})
                        end

                    true ->
                        config = proc_http_request(request, config)
                        Map.merge(config, %{buf: buf, request: %{}, state: nil})
                end
        end
    end

    def proc_http_request(request, config) do
        {_,host} = Enum.find(request.headers, & elem(&1,0)=="host")
        {http_handler,_} = get_host_handler(:http, host, request.path, config.hosts)
        {code, headers, body, config} = :erlang.apply(http_handler, :http, [request, config])

        response_bin = build_http_response(code, headers, body)

        :ok = config.transport.send(config.socket, response_bin)
        config
    end

    def proc_ws_handshake(request, config) do
        {_,host} = Enum.find(request.headers, & elem(&1,0)=="host")
        {ws_handler, opts} = get_host_handler(:ws, host, request.path, config.hosts)

        case :erlang.apply(ws_handler, :connect, [request, config]) do
            :reject ->
                response_bin = build_http_response(404, [{"Connection", "close"}], "")
                :ok = config.transport.send(config.socket, response_bin)
                config

            {:ok, config} ->
                {_,ws_key} = Enum.find(request.headers, & elem(&1,0) == "sec-websocket-key")
                {_,ws_ext} = Enum.find(request.headers, {"",""}, & elem(&1,0) == "sec-websocket-extensions")
                ws_ext = String.replace(ws_ext, " ", "")
                ws_ext = Enum.reduce(String.split(ws_ext, ""), %{}, fn(line, a)->
                    [k,v] = String.split(line,"=")
                    Map.put(a,k,v)
                end)

                extra_headers = cond do
                    opts[:compress] != nil and ws_ext["permessage-deflate"] != nil ->
                        [{"Sec-WebSocket-Extensions", "permessage-deflate"}]
                    true -> []
                end

                inject_headers = Map.get(opts, :inject_headers, [])

                reply_headers = [
                    {"Upgrade", "websocket"},
                    {"Connection", "Upgrade"},
                    {"Sec-WebSocket-Accept", :base64.encode(:crypto.hash(:sha, <<ws_key::binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>))}
                ] ++ extra_headers ++ inject_headers

                config = if length(extra_headers) > 0 do
                    inflate_zlib = :zlib.open()
                    :zlib.inflateInit(inflate_zlib, -15)

                    compress = Map.get(opts, :compress, %{})
                    level = Map.get(compress, :level, 1)
                    memLevel = Map.get(compress, :mem_level, 8)
                    windowBits = Map.get(compress, :window_bits, 15)
                    strategy = Map.get(compress, :strategy, :default)

                    deflate_zlib = :zlib.open()
                    :zlib.deflateInit(deflate_zlib, level, :deflated, -windowBits, memLevel, strategy)

                    Map.merge(config, %{inflate: inflate_zlib, deflate: deflate_zlib})
                else config end

                response_bin = build_http_response(101, reply_headers, "")
                :ok = config.transport.send(config.socket, response_bin)
                config
        end
    end

    def get_host_handler(type, host, path, config_hosts) do
        default_handler = Map.get(config_hosts, {type, "*"})
        cond do
            type == :http ->
                Map.get(config_hosts, {:http, host}, default_handler)

            type == :ws ->
                default_handler_path = Map.get(config_hosts, {:ws, {"*", path}}, default_handler)
                Map.get(config_hosts, {:ws, {host, path}}, default_handler_path)
        end
    end

    def build_http_response(code, headers, body) do
        headers = case Enum.find(headers, & elem(&1,0) == "Connection") do
            nil -> headers ++ [{"Connection", "keep-alive"}]
            _ -> headers 
        end

        headers = headers ++ [{"Content-Length", "#{byte_size(body)}"}]

        code = "#{code}"
        code_text = response_code(code)

        response_head = <<"HTTP/1.1 ", code::binary, " ", code_text::binary, "\r\n">>
        response_headers = Enum.reduce(headers, "", fn({k,v},a)-> a <> "#{k}: #{v}\r\n" end)
        <<response_head::binary, response_headers::binary, "\r\n", body::binary>>
    end

    #1×× Informational
    def response_code("100"), do: "Continue"
    def response_code("101"), do: "Switching Protocols"
    def response_code("102"), do: "Processing"

    #2×× Success
    def response_code("200"), do: "OK"
    def response_code("201"), do: "Created"
    def response_code("202"), do: "Accepted"
    def response_code("203"), do: "Non-authoritative Information"
    def response_code("204"), do: "No Content"
    def response_code("205"), do: "Reset Content"
    def response_code("206"), do: "Partial Content"
    def response_code("207"), do: "Multi-Status"
    def response_code("208"), do: "Already Reported"
    def response_code("226"), do: "IM Used"

    #3×× Redirection
    def response_code("300"), do: "Multiple Choices"
    def response_code("301"), do: "Moved Permanently"
    def response_code("302"), do: "Found"
    def response_code("303"), do: "See Other"
    def response_code("304"), do: "Not Modified"
    def response_code("305"), do: "Use Proxy"
    def response_code("307"), do: "Temporary Redirect"
    def response_code("308"), do: "Permanent Redirect"

    #4×× Client Error
    def response_code("400"), do: "Bad Request"
    def response_code("401"), do: "Unauthorized"
    def response_code("402"), do: "Payment Required"
    def response_code("403"), do: "Forbidden"
    def response_code("404"), do: "Not Found"
    def response_code("405"), do: "Method Not Allowed"
    def response_code("406"), do: "Not Acceptable"
    def response_code("407"), do: "Proxy Authentication Required"
    def response_code("408"), do: "Request Timeout"
    def response_code("409"), do: "Conflict"
    def response_code("410"), do: "Gone"
    def response_code("411"), do: "Length Required"
    def response_code("412"), do: "Precondition Failed"
    def response_code("413"), do: "Payload Too Large"
    def response_code("414"), do: "Request-URI Too Long"
    def response_code("415"), do: "Unsupported Media Type"
    def response_code("416"), do: "Requested Range Not Satisfiable"
    def response_code("417"), do: "Expectation Failed"
    def response_code("418"), do: "I'm a teapot"
    def response_code("421"), do: "Misdirected Request"
    def response_code("422"), do: "Unprocessable Entity"
    def response_code("423"), do: "Locked"
    def response_code("424"), do: "Failed Dependency"
    def response_code("426"), do: "Upgrade Required"
    def response_code("428"), do: "Precondition Required"
    def response_code("429"), do: "Too Many Requests"
    def response_code("431"), do: "Request Header Fields Too Large"
    def response_code("444"), do: "Connection Closed Without Response"
    def response_code("451"), do: "Unavailable For Legal Reasons"
    def response_code("499"), do: "Client Closed Request"

    #5×× Server Error
    def response_code("500"), do: "Internal Server Error"
    def response_code("501"), do: "Not Implemented"
    def response_code("502"), do: "Bad Gateway"
    def response_code("503"), do: "Service Unavailable"
    def response_code("504"), do: "Gateway Timeout"
    def response_code("505"), do: "HTTP Version Not Supported"
    def response_code("506"), do: "Variant Also Negotiates"
    def response_code("507"), do: "Insufficient Storage"
    def response_code("508"), do: "Loop Detected"
    def response_code("510"), do: "Not Extended"
    def response_code("511"), do: "Network Authentication Required"
    def response_code("599"), do: "Network Connect Timeout Error"

end
