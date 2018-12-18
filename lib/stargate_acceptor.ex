defmodule Stargate.Acceptor.Sup do
    def loop(config) do
        Process.flag(:trap_exit, true)
        loop_1(config)
    end

    def loop_1(config) do
        acceptors = Map.get(config,:acceptors,[])
        to_spawn = :erlang.system_info(:schedulers) - length(acceptors)
        acceptors = if to_spawn > 0 do
            pids = Enum.map(1..to_spawn, fn(_)-> :erlang.spawn_link(Stargate.Acceptor, :loop, [config]) end)
            acceptors ++ pids
        else acceptors end
        config = Map.put(config, :acceptors, acceptors)

        receive do
            {:EXIT, pid, r} ->
                IO.inspect {:stargate_acceptor_died, pid, r}
                loop_1(config)

            ukn -> 
                throw({__MODULE__, :ukn_msg, ukn})
        end
    end
end

defmodule Stargate.Acceptor do
    def loop(config) do
        {:ok, _} = :prim_inet.async_accept(config.listen_socket, -1)
        receive do
            {:inet_async, _ListenSocket, _, {:ok, csocket}} ->
                pid = :erlang.spawn(Stargate.Vessel, :loop, [config])

                :inet_db.register_socket(csocket, :inet_tcp)
                :ok = :gen_tcp.controlling_process(csocket, pid)
                send(pid, {:pass_socket, csocket})

                loop(config)

            {:inet_async, _, _, error} ->
                IO.inspect {:inet_async_error, error}
                loop(config)

            ukn -> 
                throw({__MODULE__, :ukn_msg, ukn})
        end
    end
end
