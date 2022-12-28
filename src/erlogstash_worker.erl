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
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(DEFAULT_TIMEOUT, timer:seconds(5)).
-define(RECONNECT_TIMEOUT, timer:seconds(15)).

-type handle() :: gen_tcp:socket()|gen_udp:socket()|file:fd().

-record(state, {handle::handle()|undefined, config::erlogstash:output()}).
-record(init, {count = 0::non_neg_integer(), payload = [] :: erlogstash:payload()}).

-type state() :: #state{}.
-type init() :: #init{}.

%% API

start(Output) -> gen_server:start(?MODULE, Output, []).

start_link(Output) -> gen_server:start_link(?MODULE, Output, []).

start_link(undefined, Output) -> start_link(Output);
start_link(Name, Output) when is_atom(Name) -> gen_server:start_link({local, Name}, ?MODULE, Output, []);
start_link({T, _} = Name, Output) when T =:= local; T =:= global -> gen_server:start_link(Name, ?MODULE, Output, []);
start_link({via, _, _} = Name, Output) -> gen_server:start_link(Name, ?MODULE, Output, []).

stop(Pid) -> gen_server:cast(Pid, stop).

%% gen_server callbacks

-spec init(erlogstash:output()) -> {ok, init()}.
init(Output) ->
    self() ! {reconnect, Output},
    {ok, #init{}}.

-spec handle_call(term(), {pid(), term()}, State) -> {reply, ok, State} when State :: state()|init().
handle_call(_Request, _From, State) -> {reply, ok, State}.

-spec handle_cast(term(), state() | init()) -> {stop, normal, state()} | {noreply, state()|init()}.
handle_cast(stop, State) -> {stop, normal, State};
handle_cast({log, Payload}, #init{} = Init) -> {noreply, reconnect_buf_queue(Payload, Init)};
handle_cast({log, Payload}, State) ->
    ok = send_log(Payload, State),
    {noreply, State};
handle_cast(_Msg, State) -> {noreply, State}.

-spec handle_info(term(), state()|init()) -> {stop, normal, state()} | {noreply, state()|init()}.
handle_info({reconnect, Output}, #init{} = Init) ->
    {noreply,
     case connect(Output) of
         {ok, State} -> reconnect_buf_drain(Init, State);
         {error, nxdomain} ->
             %% Keep a deliberately long timeout here to avoid thundering herds
             %% against the DNS service
             timer:send_after(timer:minutes(1), self(), {reconnect, Output}),
             Init;
         {error, Reason} ->
             %% Unknown errors should output warnings to us
             Reason =/= timeout andalso Reason =/= econnrefused andalso
                 error_logger:info_msg("Trying to connect to logstash had error reason ~p", [Reason]),
             timer:send_after(?RECONNECT_TIMEOUT, self(), {reconnect, Output}),
             Init
     end};
handle_info({tcp, S, _Data}, State) ->
    inet:setopts(S, [{active, once}]),
    {noreply, State};
handle_info({tcp_closed, S}, #state{config = Conf, handle = S}) ->
    timer:send_after(?RECONNECT_TIMEOUT, self(), {reconnect, Conf}),
    {noreply, #init{}};
handle_info({udp, S, _IP, _Port, _Data}, State) ->
    inet:setopts(S, [{active, once}]),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

%% internal functions

-spec connect(erlogstash:output()) -> {ok, state()} | {error, term()}.
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

-spec send_log(Payload::erlogstash:payload(), state()) -> ok.
send_log(Payload, #state{config = {tcp, _, _, _}, handle = Socket}) -> ok = gen_tcp:send(Socket, Payload);
send_log(Payload, #state{config = {udp, Host, Port}, handle = Socket}) ->
    ok = gen_udp:send(Socket, Host, Port, Payload);
send_log(Payload, #state{config = {file, _}, handle = Fd}) -> ok = file:write(Fd, Payload).

%% Buffer to big, cycle!
-spec reconnect_buf_queue(Payload::erlogstash:payload(), init()) -> init().
reconnect_buf_queue(Payload, #init{count = N}) when N > 500 -> #init{count = 1, payload = [Payload]};
reconnect_buf_queue(Payload, #init{count = N, payload = Payloads}) ->
    #init{count = N + 1, payload = [Payload|Payloads]}.

-spec reconnect_buf_drain(init(), state()) -> state().
reconnect_buf_drain(#init{payload = Payloads}, State) -> drain(Payloads, State).

-spec drain([erlogstash:payload()], State::state()) -> state().
drain([P|Ps], State) ->
    drain(Ps, State),
    send_log(P, State);
drain([], State) -> State.
