-module(erlogstash_fsm).

-behaviour(gen_fsm).
-compile({nowarn_deprecated_function, [{gen_fsm, start, 3},
                                       {gen_fsm, start_link, 3}, {gen_fsm, start_link, 4},
                                       {gen_fsm, send_all_state_event, 2},
                                       {gen_fsm, send_event, 2},
                                       {gen_fsm, send_event_after, 2}]}).

%% API
-export([start/1, start_link/1, start_link/2, stop/1, send/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_info/3, handle_sync_event/4, terminate/3,
         connect/2, connect/3, log/2, log/3]).

-define(DEFAULT_TIMEOUT, 5000). % milliseconds
-define(RECONNECT_TIMEOUT, 5). % seconds

-type handle() :: gen_tcp:socket()|gen_udp:socket()|file:fd().

-record(pool, {count = 0 :: non_neg_integer(), payload = [] :: [erlogstash:payload()]}).
-record(state, {handle :: handle(), output :: erlogstash:output()}).

-type pool() :: #pool{}.
-type state() :: #state{}.
-type state_data() :: pool()|state().

%% API

-spec start(Output::erlogstash:output()) -> {ok, pid()}.
start(Output) -> gen_fsm:start(?MODULE, Output, []).

-spec start_link(Output::erlogstash:output()) -> {ok, pid()}.
start_link(Output) -> gen_fsm:start_link(?MODULE, Output, []).

-spec start_link(Worker::erlogstash:worker(), Output::erlogstash:output()) -> {ok, pid()}.
start_link(undefined, Output) -> start_link(Output);
start_link(Worker, Output) when is_atom(Worker) -> gen_fsm:start_link({local, Worker}, ?MODULE, Output, []);
start_link({via, _, _} = Worker, Output) -> gen_fsm:start_link(Worker, ?MODULE, Output, []);
start_link({T, _} = Worker, Output) when T =:= local; T =:= global -> gen_fsm:start_link(Worker, ?MODULE, Output, []).

-spec stop(Worker::erlogstash:worker()) -> ok.
stop(Worker) -> gen_fsm:send_all_state_event(Worker, stop).

-spec send(Worker::erlogstash:worker(), Payload::erlogstash:payload()) -> ok.
send(Worker, Payload) -> gen_fsm:send_event(Worker, {log, Payload}).

%% gen_fsm callbacks

-define(KEEP_STATE(S), {next_state, ?FUNCTION_NAME, S}).

%% @private
-spec init(erlogstash:output()) -> {ok, connect, pool()}.
init(Output) ->
    case output(Output) of
        {ok, O} ->
            reconnect(O),
            {ok, connect, #pool{}};
        _error -> {error, {output, Output}}
    end.

-spec handle_event(stop, atom(), StateData) -> {stop, normal, StateData} when StateData::state_data();
                  (Event::term(), StateName, StateData) -> {next_state, StateName, StateData}
                    when StateName::atom(), StateData::state_data().
handle_event(stop, _, StateData) -> {stop, normal, StateData};
handle_event(Event, StateName, StateData) ->
    error_logger:warning_msg("Unknown event ~p while in state ~p", [Event, StateName]),
    {next_state, StateName, StateData}.

-spec handle_info(Info::term(), StateName::atom(), StateData::state_data()) -> {next_state, atom(), state_data()}.
handle_info({tcp, S, _Data}, StateName, #state{handle = S} = State) ->
    inet:setopts(S, [{active, once}]),
    {next_state, StateName, State};
handle_info({tcp_closed, S}, _, #state{handle = S, output = Output}) ->
    error_logger:error_msg("Connection ~p closed", [Output]),
    reconnect(?RECONNECT_TIMEOUT, Output),
    {next_state, connect, #pool{}};
handle_info({udp, S, _IP, _Port, _Data}, StateName, #state{handle = S} = State) ->
    inet:setopts(S, [{active, once}]),
    {next_state, StateName, State};
handle_info(Info, StateName, StateData) ->
    error_logger:warning_msg("Unknown info ~p while in state ~p", [Info, StateName]),
    {next_state, StateName, StateData}.

-spec handle_sync_event(Event::term(), From::{pid, atom()}, StateName, StateData) -> {next_state, StateName, StateData}
        when StateName::atom(), StateData::state_data().
handle_sync_event(Event, _From, StateName, StateData) ->
    error_logger:warning_msg("Unknown sync event ~p while in state ~p", [Event, StateName]),
    {next_state, StateName, StateData}.

-spec terminate(Reason::normal, atom(), state_data()) -> ok | {error, term()}.
terminate(_Reason, _, #state{handle = Handle, output = Output}) -> close(Handle, Output);
terminate(_Reason, _, _) -> ok.

-spec connect(Event::term(), Pool::pool()) -> {next_state, atom(), state_data()}.
connect({log, Payload}, #pool{count = N}) when N >= 500 ->
    error_logger:warning_msg("Drop ~B log events", [N]),
    ?KEEP_STATE(pool(Payload));
connect({log, Payload}, #pool{count = N, payload = Payloads}) ->
    ?KEEP_STATE(#pool{count = N + 1, payload = [Payload|Payloads]});
connect({connect, Output}, Pool) ->
    case connect(Output) of
        {ok, H} ->
            drain(H, Pool#pool.payload, Output),
            {next_state, log, #state{handle = H, output = Output}};
        {error, nxdomain} ->
            %% Keep a deliberately long timeout here to avoid thundering herds against the DNS service
            reconnect(60, Output),
            ?KEEP_STATE(Pool);
        {error, Reason} ->
            %% Unknown errors should output warnings to us
            Reason =/= timeout andalso Reason =/= econnrefused andalso
                error_logger:error_msg("Trying to connect to logstash had error reason ~p", [Reason]),
            reconnect(?RECONNECT_TIMEOUT, Output),
            ?KEEP_STATE(Pool)
    end;
connect(Event, Pool) -> handle_event(Event, ?FUNCTION_NAME, Pool).

-spec connect(Event::term(), From::{pid, atom()}, Pool) -> {next_state, connect, Pool} when Pool::pool().
connect(Event, From, Pool) -> handle_sync_event(Event, From, ?FUNCTION_NAME, Pool).

-spec log(Event::term(), State::state()) -> {next_state, atom(), state_data()}.
log({log, Payload}, #state{handle = Handle, output = Output} = State) ->
    case send_log(Handle, Payload, Output) of
        ok -> ?KEEP_STATE(State);
        {error, Reason} ->
            Reason =:= closed orelse close(Handle, Output),
            reconnect(Output),
            {next_state, connect, pool(Payload)}
    end;
log(Event, State) -> handle_event(Event, ?FUNCTION_NAME, State).

-spec log(Event::term(), From::{pid, atom()}, State) -> {next_state, loh, State} when State::state().
log(Event, From, State) -> handle_sync_event(Event, From, ?FUNCTION_NAME, State).

%% internal functions
-spec output(Output::erlogstash:output()) -> {ok, erlogstash:output()} | error.
output({file, _} = Output) -> {ok, Output};
output({udp, _, _} = Output) -> {ok, Output};
output({tcp, _, _, _} = Output) -> {ok, Output};
output({tcp, H, P}) -> {ok, {tcp, H, P, ?DEFAULT_TIMEOUT}};
output(_) -> error.

-spec reconnect(Output::erlogstash:output()) -> ok.
reconnect(Output) -> gen_fsm:send_event(self(), {connect, Output}).

-spec reconnect(T::non_neg_integer(), Output::erlogstash:output()) -> reference().
reconnect(T, Output) -> gen_fsm:send_event_after(timer:seconds(T), {connect, Output}).

-spec connect(Output::erlogstash:output()) -> {ok, handle()} | {error, term()}.
connect({tcp, Host, Port, Timeout}) -> gen_tcp:connect(Host, Port, [binary, {active, once}, {keepalive, true}], Timeout);
connect({udp, _, _}) -> gen_udp:open(0, [binary, {active, once}]);
connect({file, Path}) -> file:open(Path, [append, raw]).

-spec close(Handle::handle()|undefined, Output::erlogstash:output()) -> ok | {error, term()}.
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

-spec pool(Payload::erlogstash:payload()) -> pool().
pool(Payload) -> #pool{count = 1, payload = [Payload]}.

-spec drain(Handle::handle(), [erlogstash:payload()], Output::erlogstash:output()) -> ok.
drain(Handle, [P|Ps], Output) ->
    drain(Handle, Ps, Output),
    send_log(Handle, P, Output);
drain(_, [], _) -> ok.
