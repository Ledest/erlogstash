-module(erlogstash).

-behaviour(application).
-behaviour(supervisor).

%% API
-export([send/2, start_link/0, start_worker/1, start_worker/2, stop_worker/1]).
%% application callbacks
-export([start/2, stop/1]).
%% supervisor callbacks
-export([init/1]).

-type output() :: {file, file:name_all()|iodata()} |
                  {udp, inet:hostname()|inet:ip_address(), inet:port_number()} |
                  {tcp, inet:hostname()|inet:socket_address(), inet:port_number()} |
                  {tcp, inet:hostname()|inet:socket_address(), inet:port_number(), timeout()}.
-type payload() :: iodata().
-type worker() :: supervisor:sup_ref().
-export_type([output/0, payload/0, worker/0]).

%% API

-spec send(Worker::worker(), Payload::payload()) -> ok.
send(Worker, Payload) -> erlogstash_fsm:send(Worker, Payload).

%% @doc
%% Starts the supervisor
%% @end
-spec start_link() -> supervisor:startlink_ret().
start_link() -> supervisor:start_link({local, erlogstash_sup}, ?MODULE, []).

-spec start_worker(Output::output()) -> {ok, pid()} | {error, supervisor:startchild_err()}.
start_worker(Output) -> supervisor:start_child(erlogstash_sup, [Output]).

-spec start_worker(Worker::worker() | {local, atom()}, Output::output()) ->
          {ok, pid()} | {error, supervisor:startchild_err()}.
start_worker(Worker, Output) -> supervisor:start_child(erlogstash_sup, [Worker, Output]).

-spec stop_worker(Worker::worker()) -> ok.
stop_worker(Worker) -> erlogstash_server:stop(Worker).

%% application callbacks

%% @private
-spec start(application:start_type(), term()) -> {ok, pid()} | {error, supervisor:startlink_err()}.
start(_StartType, _StartArgs) ->
    case start_link() of
        {ok, _} = R ->
            lists:foreach(fun({N, O}) -> start_worker(N, O) end, application:get_env(erlogstash, outputs, [])),
            ok = logger:add_handlers(erlogstash),
            R;
        R -> R
    end.

%% @private
-spec stop(State::[]) -> ok.
stop(_) -> ok.

%% supervisor callbacks

%% @private
-spec init([]) -> {ok, {{simple_one_for_one, non_neg_integer(), 1..1000000}, [supervisor:child_spec()]}}.
init([]) ->
    {ok,
     {#{strategy => simple_one_for_one, intensity => 50, period => 3600},
      [#{id => undefined, start => {erlogstash_fsm, start_link, []}, restart => transient, shutdown => 2000}]}}.
