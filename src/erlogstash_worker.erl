%% Copyright (c) 2014 Krzysztof Rutka
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%% IN THE SOFTWARE.

%% @author Chap Lovejoy <chaplovejoy@gmail.com>

-module(erlogstash_worker).

-behaviour(gen_server).

%% API
-export([start/1, start_link/1, start_link/2, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(DEFAULT_TIMEOUT, timer:seconds(5)).
-define(RECONNECT_TIMEOUT, timer:seconds(15)).

-type handle() :: gen_tcp:socket() | gen_udp:socket() | file:fd().
-type config() :: {tcp, inet:hostname(), inet:port_number(), pos_integer()} |
                  {tcp | udp, inet:hostname(), inet:port_number()} |
                  {file, string()}.

-record(state, {handle :: handle() | undefined, config :: config()}).

%%%===================================================================
%%% API
%%%===================================================================

start(Output) -> gen_server:start(?MODULE, [Output], []).

start_link(Output) -> gen_server:start_link(?MODULE, [Output], []).

start_link(undefined, Output) -> start_link(Output);
start_link(Name, Output) when is_atom(Name) -> gen_server:start_link({local, Name}, ?MODULE, [Output], []);
start_link({T, _} = Name, Output) when T =:= local; T =:= global -> gen_server:start_link(Name, ?MODULE, [Output], []);
start_link({via, _, _} = Name, Output) -> gen_server:start_link(Name, ?MODULE, [Output], []).

stop(Pid) -> gen_server:cast(Pid, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Output]) ->
    self() ! {reconnect, Output},
    {ok, reconnect_buf_init()}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) -> {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) -> {stop, normal, State};
handle_cast({log, Payload}, {initializing, _} = State) -> {noreply, reconnect_buf_queue(Payload, State)};
handle_cast({log, Payload}, State) ->
    ok = send_log(Payload, State),
    {noreply, State};
handle_cast(_Msg, State) -> {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({reconnect, Output}, {initializing, _} = BufState) ->
    {noreply,
     case connect(Output) of
         {ok, State} -> reconnect_buf_drain(BufState, State);
         {error, nxdomain} ->
             %% Keep a deliberately long timeout here to avoid thundering herds
             %% against the DNS service
             timer:send_after(timer:minutes(1), self(), {reconnect, Output}),
             BufState;
         {error, Reason} ->
             %% Unknown errors should output warnings to us
             Reason =/= timeout andalso Reason =/= econnrefused andalso
                 error_logger:info_msg("Trying to connect to logstash had error reason ~p", [Reason]),
             timer:send_after(?RECONNECT_TIMEOUT, self(), {reconnect, Output}),
             BufState
     end};
handle_info({tcp, S, _Data}, State) ->
    inet:setopts(S, [{active, once}]),
    {noreply, State};
handle_info({tcp_closed, S}, #state {config = Conf, handle = S}) ->
    timer:send_after(?RECONNECT_TIMEOUT, self(), {reconnect, Conf}),
    {noreply, reconnect_buf_init()};
handle_info({udp, S, _IP, _Port, _Data}, State) ->
    inet:setopts(S, [{active, once}]),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) -> ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

connect({tcp, Host, Port}) -> connect({tcp, Host, Port, ?DEFAULT_TIMEOUT});
connect({tcp, Host, Port, Timeout} = Conf) ->
    case gen_tcp:connect(Host, Port, [binary, {active, once}, {keepalive, true}], Timeout) of
        {ok, Socket} -> {ok, #state{config = Conf, handle = Socket}};
        {error, _} = R -> R
    end;
connect({udp, _, _} = Conf) ->
    case gen_udp:open(0, [binary]) of
        {ok, Socket} -> {ok, #state{config = Conf, handle = Socket}};
        {error, _} = R -> R
    end;
connect({file, Path} = Conf) ->
    case file:open(Path, [append]) of
        {ok, Fd} -> {ok, #state{config = Conf, handle = Fd}};
        {error, _} = R -> R
    end.

send_log(Payload, #state{config = {tcp, _, _, _}, handle = Socket}) -> ok = gen_tcp:send(Socket, Payload);
send_log(Payload, #state{config = {udp, Host, Port}, handle = Socket}) ->
    ok = gen_udp:send(Socket, Host, Port, Payload);
send_log(Payload, #state{config = {file, _}, handle = Fd}) -> ok = file:write(Fd, Payload).

%% -- Reconnect Buffering ---------------------------------------
reconnect_buf_init() -> {initializing, {0, []}}.

reconnect_buf_queue(Payload, {initializing, {N, _Msgs}}) when N > 500 ->
    %% Buffer to big, cycle!
    {initializing, {1, [Payload]}};
reconnect_buf_queue(Payload, {initializing, {N, Msgs}}) -> {initializing, {N + 1, [Payload|Msgs]}}.

reconnect_buf_drain({initializing, {_N, Ps}}, State) -> drain(Ps, State).

drain([P|Ps], State) ->
    drain(Ps, State),
    send_log(P, State);
drain([], State) -> State.