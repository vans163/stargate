-module(stargate_handler_wildcard_ws).
-behavior(gen_server).
-compile(export_all).

-import(stargate_transport, [peername/1]).

handle_cast(_Message, S) -> {noreply, S}.
handle_call(_Message, _From, S) -> {reply, ok, S}.
code_change(_OldVersion, S, _Extra) -> {ok, S}. 

start_link(Params) -> gen_server:start_link(?MODULE, Params, []).

init({ParentPid, Query, Headers, State}) ->
    process_flag(trap_exit, true),

    Socket = maps:get(socket, State),
    {ok, {SourceAddr, _}} = peername(Socket),
    io:format("~p:~n connect~n ~p~n ~p~n ~p~n ~p~n ~p~n", 
        [?MODULE, ParentPid, self(), Query, Headers, SourceAddr]),
    {ok, State#{parent=> ParentPid}}.

terminate(Reason, _S) -> 
    io:format("~p:~n disconnect~n ~p~n", [?MODULE, Reason]).

handle_info({'EXIT', _, _Reason}, D) ->
    {stop, {shutdown, got_exit_signal}, D};



handle_info({text, Bin}, S) ->
    io:format("~p:~n Got text~n ~p~n", [?MODULE, Bin]),
    {noreply, S};

handle_info({bin, Bin}, S) ->
    io:format("~p:~n Got bin~n ~p~n", [?MODULE, Bin]),
    {noreply, S};

handle_info(Message, S) -> 
    io:format("~p:~n Unhandled handle_info~n ~p~n ~p~n", [?MODULE, Message, S]),
    {noreply, S}.