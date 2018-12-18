defmodule Stargate.Handler.Wildcard.Http.Redirect do
    def http(%{headers: h, path: path, query: query}, s) do
        {_,host} = Enum.find(h, & elem(&1,0) == "host")

        query = Enum.reduce(query, "", fn({k,v},a)-> "#{a}&#{k}=#{v}" end)
        query = if byte_size(query) > 0, do: "?#{String.trim_leading(query, "&")}", else: ""

        ssl_path = "https://#{host}/#{path}#{query}"
        {301, [{"Location", ssl_path}], "", s}
    end
end

defmodule Stargate.Handler.Wildcard.Http do
    def http(%{path: path}, s) do
        #IO.inspect {"unhandled http", __MODULE__, path}
        {200, [], "", s}
    end
end

defmodule Stargate.Handler.Wildcard.WS do
    def connect(%{path: path}, s) do
        IO.inspect {"unhandled ws connect", __MODULE__, path}
        {:ok, s}
    end

    def payload(bin, s) do
        IO.inspect {"unhandled ws payload", __MODULE__, bin}
        s        
    end
end
