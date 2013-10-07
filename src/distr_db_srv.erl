-module(distr_db_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    Storage = ets:new(storage, [set, public, named_table]),
    case gen_tcp:listen(8082, ?TCP_OPTIONS) of
        {ok, LSocket} ->
            State = [0, Storage, LSocket, Servers],
            {ok, accept(State)};
        {error, Reason} ->
            {stop, Reason}
    end.

accept(State) ->
    proc_lib:spawn(?MODULE, accept_loop, [State]),
    State.

accept_loop([_Time, _Stor, LSocket, Servers]) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    gen_server:cast(?MODULE, loop), %%
    case gen_tcp:recv(Socket, 0) of
        {ok, {put, _Key, _Data, _Time, _ServId} = Message} ->
            gen_server:call(?MODULE, Message);
        {ok, {get, all, _ServId} = Message} ->
            gen_server:call(?MODULE, Message);
        {error, closed} ->
            ok
    end. 

handle_call({get, Key}, _From, State = [_Time, Stor, _LSocket, _Servers]) ->
    [{Key, {_ServId, _Time, Data}}] = ets:lookup(Stor, Key),
    {reply, {ok, Value}, State};


handle_call({get, all, ServId}, _From, State = [_Time, Stor, _LSocket, _Servers]) -> %% от другого сервера

    {reply, {ok, <<"hst">>}, State};


handle_call({put, Key, Data}, _From, _State = [_Time, Stor, LSocket, {MyId, ServersList}]) ->
    {Mega, Seconds, _} = erlang:now(),
    Time = Mega * 1000000 + Seconds,

    ets:insert(Stor, {Key, {MyId, Time, Data}}), %% добавить проверки при вставке

    send_servers({put, Key, Data, Time, MyId}, ServersList),

    NewState = [Time, Stor, LSocket, Servers],
    {reply, ok, NewState};



handle_call({put, Key, Data, Time, ServId}, _From, State = [_Time, Stor, _LSocket, _Servers]) -> %% от другого сервера
    ets:insert(Stor, {Key, {ServId, Time, Data}}), %% те же проверки 

    {reply, ok, State};


handle_call(_Request, _From, State) ->
    {reply, error, State}.

handle_cast(loop, State) ->
    {noreply, accept(State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

send_servers(Data, Servers) ->
    lists:foreach(fun({_Id, Host, Port}) -> 
                          case gen_tcp:connect((Host, Port, [{active, false}, {packet,2}])) of
                              {ok, Sock} -> 
                                  gen_tcp:send(Sock, Data),
                                  gen_tcp:close(Sock);
                              {error, Reason} -> 
                                  {error, Reason}
                          end
                  end, Servers).
