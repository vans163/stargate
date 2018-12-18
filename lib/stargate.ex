defmodule Stargate do
    def fix_config(config) do
        if config[:ssl_opts] != nil do
            :ssl.start()
        end

        hosts = Map.get(config, :hosts, %{})
        hosts = if !hosts[{:http, "*"}] do
            Map.put(hosts, {:http, "*"}, {Stargate.Handler.Wildcard.Http, %{}})
        else hosts end
        hosts = if !hosts[{:ws, "*"}] do
            Map.put(hosts, {:ws, "*"}, {Stargate.Handler.Wildcard.WS, %{}})
        else hosts end
        Map.put(config, :hosts, hosts)
    end

    def warp_in(config) do
        config = fix_config(config)

        if elem(config.ip,0) == :local do
            path = elem(config.ip,1)
            File.rm(path)
        end

        listen_args = Map.get(config, :listen_args, [])
        {:ok, lsocket} = :gen_tcp.listen(config.port, listen_args ++ [
            {:ifaddr, config.ip}, {:active, false}, {:reuseaddr, true}, {:nodelay, true}, {:recbuf, 4096}, {:exit_on_close, false}, :binary
        ])
        config = Map.merge(config, %{listen_socket: lsocket, buf: <<>>})

        :erlang.spawn(Stargate.Acceptor.Sup, :loop, [config])
    end
end
