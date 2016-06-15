
-define(HANDLER_WILDCARD, handler_wildcard).

-define(TIMEOUT, 30000).
-define(MAX_TCP_TIMEOUT, 120000).

-define(MAX_HEADER_SIZE, 8000).
-define(MAX_BODY_SIZE, 20000).



-ifndef(PRINT).
-define(PRINT(Var), io:format("~p:~p - ~p~n", [?MODULE, ?LINE, Var])).
-endif.