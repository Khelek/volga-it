-module(distr_db).


-export([start/0, stop/0]).

start() ->
    application:start(ranch),
    application:start(crypto),
    application:start(cowlib),
    application:start(cowboy),
    application:start(distr_db).

stop() ->
    ok.
