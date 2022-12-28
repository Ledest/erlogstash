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
-export_type([output/0, payload/0]).

%% API

-spec send(Worker::supervisor:sup_ref(), Payload::payload()) -> ok.
send(Worker, Payload) -> gen_server:cast(Worker, {log, Payload}).

%% @doc
%% Starts the supervisor
%% @end
-spec start_link() -> supervisor:startlink_ret().
start_link() -> supervisor:start_link({local, erlogstash_sup}, ?MODULE, []).

-spec start_worker(Output::output()) -> {ok, pid()} | {error, supervisor:startchild_err()}.
start_worker(Output) -> supervisor:start_child(erlogstash_sup, [Output]).

-spec start_worker(Name::supervisor:sup_ref() | {local, atom()}, Output::output()) ->
          {ok, pid()} | {error, supervisor:startchild_err()}.
start_worker(Name, Output) -> supervisor:start_child(erlogstash_sup, [Name, Output]).

-spec stop_worker(Worker::supervisor:sup_ref()) -> ok.
stop_worker(Worker) -> gen_server:cast(Worker, stop).

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
    {ok, {#{strategy => simple_one_for_one, intensity => 50, period => 3600},
          [#{id => undefined, start => {erlogstash_worker, start_link, []}, restart => transient, shutdown => 2000}]}}.
