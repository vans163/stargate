-module(stargate_sup).
-behaviour(supervisor).
-compile(export_all).

start_link() -> 
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Spec = {
        stargate_child_gen, {stargate_child_gen, start_link, []}, 
            permanent, 5000, worker, [stargate_child_gen]
    },

    {ok,
        {{simple_one_for_one, 2, 10}, [Spec]}
    }.

child_pids() ->
    Children = supervisor:which_children(?MODULE),
    [P || {undefined,P,worker,[stargate_child_gen]} <- Children].

start_child(StargateArgs) ->
    supervisor:start_child(?MODULE, [StargateArgs]).

terminate_child(ChildPid) ->
    supervisor:terminate_child(?MODULE, ChildPid).