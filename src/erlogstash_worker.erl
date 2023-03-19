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
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(DEFAULT_TIMEOUT, timer:seconds(5)).
-define(RECONNECT_TIMEOUT, timer:seconds(5)).

-type handle() :: gen_tcp:socket()|gen_udp:socket()|file:fd().

-record(state, {handle :: handle()|undefined, output :: erlogstash:output()}).
-record(init, {count = 0 :: non_neg_integer(), payload = [] :: erlogstash:payload()}).

-type state() :: #state{}.
-type init() :: #init{}.

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

%% gen_server callbacks

%% @private
-spec init(erlogstash:output()) -> {ok, init()}.
init(Output) ->
    self() ! {reconnect, Output},
    {ok, #init{}}.

%% @private
-spec handle_call(term(), {pid(), term()}, State) -> {reply, ok, State} when State :: state()|init().
handle_call(_Request, _From, State) -> {reply, ok, State}.

%% @private
-spec handle_cast(term(), state() | init()) -> {stop, normal, state()} | {noreply, state()|init()}.
handle_cast(stop, State) -> {stop, normal, State};
handle_cast({log, Payload}, #init{} = Init) -> {noreply, reconnect_buf_queue(Payload, Init)};
handle_cast({log, Payload}, #state{handle = Handle, output = Output} = State) ->
    {noreply,
     case send_log(Handle, Payload, Output) of
         ok -> State;
         {error, Reason} ->
             Reason =:= closed orelse close(Handle, Output),
             self() ! {reconnect, Output},
             #init{count = 1, payload = [Payload]}
     end};
handle_cast(_Msg, State) -> {noreply, State}.

%% @private
-spec handle_info(term(), state()|init()) -> {noreply, state()|init()}.
handle_info({reconnect, Output}, #init{} = Init) ->
    {noreply,
     case connect(Output) of
         {ok, #state{handle = H, output = O} = State} ->
             drain(H, Init#init.payload, O),
             State;
         {error, nxdomain} ->
             %% Keep a deliberately long timeout here to avoid thundering herds
             %% against the DNS service
             timer:send_after(timer:minutes(1), self(), {reconnect, Output}),
             Init;
         {error, Reason} ->
             %% Unknown errors should output warnings to us
             Reason =/= timeout andalso Reason =/= econnrefused andalso
                 error_logger:error_msg("Trying to connect to logstash had error reason ~p", [Reason]),
             timer:send_after(?RECONNECT_TIMEOUT, self(), {reconnect, Output}),
             Init
     end};
handle_info({tcp, S, _Data}, State) ->
    inet:setopts(S, [{active, once}]),
    {noreply, State};
handle_info({tcp_closed, S}, #state{output = Output, handle = S}) ->
    error_logger:error_msg("Connection ~p closed", [Output]),
    timer:send_after(?RECONNECT_TIMEOUT, self(), {reconnect, Output}),
    {noreply, #init{}};
handle_info({udp, S, _IP, _Port, _Data}, State) ->
    inet:setopts(S, [{active, once}]),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

-spec terminate(Reason::normal, State::state()) -> ok | {error, term()}.
terminate(_Reason, #state{handle = Handle, output = Output}) -> close(Handle, Output).

%% internal functions

-spec connect(Output::erlogstash:output()) -> {ok, state()} | {error, term()}.
connect({tcp, Host, Port}) -> connect({tcp, Host, Port, ?DEFAULT_TIMEOUT});
connect({tcp, Host, Port, Timeout} = Output) ->
    case gen_tcp:connect(Host, Port, [binary, {active, once}, {keepalive, true}], Timeout) of
        {ok, Socket} -> {ok, #state{output = Output, handle = Socket}};
        {error, _} = R -> R
    end;
connect({udp, _, _} = Output) ->
    case gen_udp:open(0, [binary]) of
        {ok, Socket} -> {ok, #state{output = Output, handle = Socket}};
        {error, _} = R -> R
    end;
connect({file, Path} = Output) ->
    case file:open(Path, [append]) of
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
            error_logger:error_msg("Send ~p: ~p", [Output, E]),
            E
    end.

-spec send(Handle::handle(), Payload::erlogstash:payload(), Output::erlogstash:output()) -> ok | {error, term()}.
send(Handle, Payload, {tcp, _, _, _}) -> gen_tcp:send(Handle, Payload);
send(Handle, Payload, {udp, Host, Port}) ->
    case gen_udp:send(Handle, Host, Port, Payload) of
        {error, emsgsize} -> error_logger:error_msg(?MODULE_STRING ": UDP message size ~B", [iolist_size(Payload)]);
        R -> R
    end;
send(Handle, Payload, {file, _}) -> file:write(Handle, Payload).

%% Buffer to big, cycle!
-spec reconnect_buf_queue(Payload::erlogstash:payload(), init()) -> init().
reconnect_buf_queue(Payload, #init{count = N}) when N >= 500 ->
    error_logger:warning_msg("Drop ~B log events", [N]),
    #init{count = 1, payload = [Payload]};
reconnect_buf_queue(Payload, #init{count = N, payload = Payloads}) ->
    #init{count = N + 1, payload = [Payload|Payloads]}.

-spec drain(Handle::handle(), [erlogstash:payload()], Output::erlogstash:output()) -> ok.
drain(Handle, [P|Ps], Output) ->
    drain(Handle, Ps, Output),
    send_log(Handle, P, Output);
drain(_, [], _) -> ok.
