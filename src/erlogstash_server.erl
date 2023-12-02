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

-module(erlogstash_server).

-behaviour(gen_server).

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 21).
-include_lib("kernel/include/logger.hrl").
-endif.
-endif.

%% API
-export([start/1, start_link/1, start_link/2, stop/1, send/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-ifndef(LOG_ERROR).
-define(LOG_ERROR(F, A), error_logger:error_msg(F, A)).
-endif.
-ifndef(LOG_WARNING).
-define(LOG_WARNING(F, A), error_logger:warning_msg(F, A)).
-endif.
-ifndef(LOG_NOTICE).
-define(LOG_NOTICE(F, A), error_logger:info_msg(F, A)).
-endif.

-define(DEFAULT_TIMEOUT, 5000). % milliseconds
-define(RECONNECT_TIMEOUT, 5). % seconds

-type handle() :: gen_tcp:socket()|gen_udp:socket()|file:fd().

-record(state, {handle :: handle()|undefined, output :: erlogstash:output()}).
-record(pool, {count = 0 :: non_neg_integer(), payload = [] :: [erlogstash:payload()]}).

-type state() :: #state{}.
-type pool() :: #pool{}.
-type state_data() :: state()|pool().

%% API

-spec start(Output::erlogstash:output()) -> {ok, pid()}.
start(Output) -> gen_server:start(?MODULE, Output, []).

-spec start_link(Output::erlogstash:output()) -> {ok, pid()}.
start_link(Output) -> gen_server:start_link(?MODULE, Output, []).

-spec start_link(Worker::erlogstash:worker(), Output::erlogstash:output()) -> {ok, pid()}.
start_link(undefined, Output) -> start_link(Output);
start_link(Worker, Output) when is_atom(Worker) -> gen_server:start_link({local, Worker}, ?MODULE, Output, []);
start_link({via, _, _} = Worker, Output) -> gen_server:start_link(Worker, ?MODULE, Output, []);
start_link({T, _} = Worker, Output) when T =:= local; T =:= global ->
    gen_server:start_link(Worker, ?MODULE, Output, []).

-spec stop(Worker::erlogstash:worker()) -> ok.
stop(Worker) -> gen_server:cast(Worker, stop).

-spec send(Worker::erlogstash:worker(), Payload::erlogstash:payload()) -> ok.
send(Worker, Payload) -> gen_server:cast(Worker, {log, Payload}).

%% gen_server callbacks

%% @private
-spec init(erlogstash:output()) -> {ok, pool()} | {error, {output, term()}}.
init(Output) ->
    case output(Output) of
        {ok, O} ->
            reconnect(O),
            {ok, #pool{}};
        _error -> {error, {output, Output}}
    end.

%% @private
-spec handle_call(term(), {pid(), term()}, State) -> {reply, ok, State} when State :: state_data().
handle_call(_Request, _From, State) -> {reply, ok, State}.

%% @private
-spec handle_cast(term(), state() | pool()) -> {stop, normal, state()} | {noreply, state_data()}.
handle_cast(stop, State) -> {stop, normal, State};
handle_cast({log, Payload}, #pool{count = N}) when N >= 500 -> % Buffer to big, cycle!
    ?LOG_WARNING("Drop ~B log events", [N]),
    {noreply, #pool{count = 1, payload = [Payload]}};
handle_cast({log, Payload}, #pool{count = N, payload = Payloads}) ->
    {noreply, #pool{count = N + 1, payload = [Payload|Payloads]}};
handle_cast({log, Payload}, #state{handle = Handle, output = Output} = State) ->
    {noreply,
     case send_log(Handle, Payload, Output) of
         ok -> State;
         {error, Reason} ->
             Reason =:= closed orelse close(Handle, Output),
             reconnect(Output),
             #pool{count = 1, payload = [Payload]}
     end};
handle_cast(_Msg, State) -> {noreply, State}.

%% @private
-spec handle_info(term(), state_data()) -> {noreply, state_data()}.
handle_info({reconnect, Output}, #pool{} = Pool) ->
    {noreply,
     case connect(Output) of
         {ok, #state{handle = H, output = O} = State} ->
             drain(H, Pool#pool.payload, O),
             ?LOG_NOTICE("Erlogstash connected ~p", [Output]),
             State;
         {error, nxdomain} ->
             %% Keep a deliberately long timeout here to avoid thundering herds against the DNS service
             reconnect(60, Output),
             Pool;
         {error, Reason} ->
             %% Unknown errors should output warnings to us
             Reason =/= timeout andalso Reason =/= econnrefused andalso
                 ?LOG_ERROR("Trying to connect to logstash had error reason ~p", [Reason]),
             reconnect(?RECONNECT_TIMEOUT, Output),
             Pool
     end};
handle_info({tcp, S, _Data}, #state{handle = S} = State) ->
    inet:setopts(S, [{active, once}]),
    {noreply, State};
handle_info({tcp_closed, S}, #state{output = Output, handle = S}) ->
    ?LOG_ERROR("Erlogstash connection ~p closed", [Output]),
    reconnect(?RECONNECT_TIMEOUT, Output),
    {noreply, #pool{}};
handle_info({udp, S, _IP, _Port, _Data}, #state{handle = S} = State) ->
    inet:setopts(S, [{active, once}]),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

%% @private
-spec terminate(Reason::normal, State::state_data()) -> ok | {error, term()}.
terminate(_Reason, #state{handle = Handle, output = Output}) -> close(Handle, Output);
terminate(_Reason, #pool{}) -> ok.

%% internal functions
-spec output(Output::erlogstash:output()) -> {ok, erlogstash:output()} | error.
output({file, _} = Output) -> {ok, Output};
output({udp, _, _} = Output) -> {ok, Output};
output({tcp, _, _, _} = Output) -> {ok, Output};
output({tcp, H, P}) -> {ok, {tcp, H, P, ?DEFAULT_TIMEOUT}};
output(_) -> error.

-spec reconnect(Output) -> {reconnect, Output} when Output::erlogstash:output().
reconnect(Output) -> self() ! {reconnect, Output}.

-spec reconnect(T::pos_integer(), Output::erlogstash:output()) -> reference().
reconnect(T, Output) -> send_after(T, {reconnect, Output}).

-spec send_after(T::pos_integer(), M::term()) -> reference().
send_after(T, M) -> erlang:send_after(timer:seconds(T), self(), M).

-spec connect(Output::erlogstash:output()) -> {ok, state()} | {error, term()}.
connect({tcp, Host, Port, Timeout} = Output) ->
    case gen_tcp:connect(Host, Port, [binary, {active, once}, {keepalive, true}], Timeout) of
        {ok, Socket} -> {ok, #state{output = Output, handle = Socket}};
        {error, _} = R -> R
    end;
connect({udp, Host, Port} = Output) ->
    case gen_udp:open(0, [binary, {active, once}]) of
        {ok, Socket} ->
            case gen_udp:connect(Socket, Host, Port) of
                ok -> {ok, #state{output = Output, handle = Socket}};
                {error, _} = R -> R
            end;
        {error, _} = R -> R
    end;
connect({file, Path} = Output) ->
    case file:open(Path, [append, raw]) of
        {ok, Fd} -> {ok, #state{output = Output, handle = Fd}};
        {error, _} = R -> R
    end.

-spec close(Handle::handle()|undefined, Output::erlogstash:output()) -> ok | {error, term()}.
close(undefined, _) -> ok;
close(Handle, {tcp, _, _, _}) -> gen_tcp:close(Handle);
close(Handle, {udp, _, _}) -> gen_udp:close(Handle);
close(Handle, {file, _}) -> file:close(Handle);
close(_, _) -> ok.

-spec send_log(Handle::handle(), Payload::erlogstash:payload(), Output::erlogstash:output()) -> ok | {error, term()}.
send_log(Handle, Payload, Output) ->
    case send(Handle, Payload, Output) of
        ok -> ok;
        {error, _} = E ->
            ?LOG_ERROR("Send ~p: ~p", [Output, E]),
            E
    end.

-spec send(Handle::handle(), Payload::erlogstash:payload(), Output::erlogstash:output()) -> ok | {error, term()}.
send(Handle, Payload, {tcp, _, _, _}) -> gen_tcp:send(Handle, Payload);
send(Handle, Payload, {udp, _, _}) ->
    case gen_udp:send(Handle, Payload) of
        {error, emsgsize} -> ?LOG_ERROR("UDP message size ~B", [iolist_size(Payload)]);
        R -> R
    end;
send(Handle, Payload, {file, _}) -> file:write(Handle, Payload).

-spec drain(Handle::handle(), [erlogstash:payload()], Output::erlogstash:output()) -> ok.
drain(Handle, [P|Ps], Output) ->
    drain(Handle, Ps, Output),
    send_log(Handle, P, Output);
drain(_, [], _) -> ok.
