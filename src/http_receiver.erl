-module(http_receiver).

%% API.
-export([start/0]).
-export([stop/0]).

%% cowboy handler
-export([init/3, handle/2, terminate/3]).
%% API.

start() ->
    Dispatch = cowboy_router:compile([
                                      {'_', [
                                             {"/data/:key", ?MODULE, []}
                                            ]}
                                     ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8081}], [
                                                            {env, [{dispatch, Dispatch}]}
                                                           ]).

stop() ->
    ok.

%% cowboy handler

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {Key, Req2} = cowboy_req:binding(key, Req2),
    % if_longer_key(Key), %% exit
    Req4 = case Method of 
               <<"PUT">> ->
                   {ok, Data, Req3} = cowboy_req:body(16384, Req2), %% TODO: проверить, падает ли хендлер без восстановления. мб поставить case  
                   % HasBody = cowboy_req:has_body(Req2), % если тело пустое - удалять по ключу
                   ok = put_to_db(Key, Data), 
                   send(<<"ok">>, Req3),
                   Req3; 
               <<"GET">> ->
                   Data = get_from_db(Key),
                   send(Data, Req2),
                   Req2;
               _ ->
                   send_error(Req2),
                   Req2
           end,
    {ok, Req4, State}.

terminate(_Reason, _Req, _State) ->
    ok.

%% utilite function

put_to_db(Key, Data) ->
    ok = gen_server:call(distr_db_srv, {put, Key, Data}).

get_from_db(Key) ->
    {ok, Data} = gen_server:call(distr_db_srv, {get, Key}),
    Data.

send_error(Req) ->
    %% Method not allowed.
    cowboy_req:reply(405, Req).

send(Data, Req) ->
    cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}], Data, Req).
