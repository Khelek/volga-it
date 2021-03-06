-module(distr_db_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    distr_db_srv:start_link(),
    http_receiver:start(),
    distr_db_sup:start_link().

stop(_State) ->
    ok.
