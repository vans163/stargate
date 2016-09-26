-module(stargate_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).


start_link() -> 
    supervisor:start_link(?MODULE, []).

init([]) ->
    Spec = {
        stargate_child, {stargate_child, start_link, []}, 
            permanent, 5000, worker, [stargate_child]
    },

    {ok,
        {{simple_one_for_one, 0, 1}, [Spec]}
    }.


start_child(SupPid, StargateArgs) ->
    supervisor:start_child(SupPid, [StargateArgs]).

delete_child(SupPid, ChildPid) ->
    case supervisor:get_childspec(SupPid, ChildPid) of
        {error, not_found} -> not_found;
        {ok, #{id:= Id}} -> 
            case supervisor:terminate_child(SupPid, ChildPid) of
                {error, E} -> {error, E};
                ok ->
                    case supervisor:delete_child(SupPid, Id) of
                        {error, E} -> {error, E};
                        ok -> ok
                    end
            end
    end.