-module(http_receiver).

-export([start/1]).

start(Port) ->
    {ok, Sock} = gen_tcp:listen(Port, [list, {active, false}, {packet, http}]),
    loop(Sock),
    ok.

loop(Sock) ->
    {ok, Conn} = gen_tcp:accept(Sock),
    spawn(fun() -> handle_request(Conn) end),
    loop(Sock).

handle_request(Conn) ->
    {ok, {http_request, Method, Path, Version}}=gen_tcp:recv(Conn, 0),
    case (Method) of
        'PUT' ->
            gen_tcp:send(Conn, "shtsht");
           % gen_server:cast();
        'GET' ->
            gen_tcp:send(Conn, "sht");
        _ -> gen_tcp:send(Conn, "Error")
    end,
    gen_tcp:close(Conn).
