-module(stargate_acceptor_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Params) ->
    supervisor:start_link(?MODULE, Params).

init(_Params={StargateArgs, ListenSocket}) ->

    AcceptorCount = erlang:system_info(schedulers),
    Acceptors = [
        {
            {acceptor, self(), N}, 
            {stargate_acceptor, start_link, [{StargateArgs, ListenSocket, N}]}, 
            permanent, 5000, worker, []
        } || N <- lists:seq(1, AcceptorCount)
    ],

    {ok,
        { 
            {one_for_one, 2, 10},
            Acceptors
        }
    }.