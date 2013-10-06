-module(http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    HasBody = cowboy_req:has_body(Req2),
    {ok, Req3} = maybe_echo(Method, HasBody, Req2),
    {ok, Req3, State}.

maybe_echo(<<"PUT">>, true, Req) ->
    {ok, PostVals, Req2} = cowboy_req:body_qs(Req),
    Echo = proplists:get_value(<<"echo">>, PostVals),
    echo(Echo, Req2);
maybe_echo(<<"PUT">>, false, Req) ->
    cowboy_req:reply(400, [], <<"Missing body.">>, Req);
maybe_echo(<<"GET">>, _, Req) ->
    {Echo, Req2} = cowboy_req:qs_val(<<"echo">>, Req),
    echo(Echo, Req2);
maybe_echo(_, _, Req) ->
    %% Method not allowed.
    cowboy_req:reply(405, Req).

echo(undefined, Req) ->
    cowboy_req:reply(400, [], <<"Missing echo parameter.">>, Req);
echo(Echo, Req) ->
    {Key, Req} = cowboy_req:binding(key, Req),
    cowboy_req:reply(200, [
                           {<<"content-type">>, <<"text/plain; charset=utf-8">>}
                          ], Echo, Req).

terminate(_Reason, _Req, _State) ->
    ok.
