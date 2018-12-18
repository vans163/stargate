defmodule Stargate.Mixfile do
    use Mix.Project

    def project, do: [
        app: :stargate,
        version: "0.0.1",
        elixir: "~> 1.6",
        build_embedded: Mix.env == :prod,
        start_permanent: Mix.env == :prod,
        deps: deps(),
    ]

    def deps, do: [
    ]
end
