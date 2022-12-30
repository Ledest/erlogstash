-module(logger_erlogstash_formatter).

%% formatter callbacks
-export([check_config/1, format/2]).

-define(DEFAULT_FORMAT, json).

-type format() :: json|msgpack.
-type data() :: [{atom(), null|atom()|number()|binary()|[atom()]}].
-type msg() :: {io:format(), [term()]} | {report, logger:report()} | {string, unicode:chardata()}.

-spec check_config(Config::logger_formatter:config()) -> ok | {error, term()}.
check_config(#{format := F}) when F =/= json, F =/= msgpack -> {error, {format, F}};
check_config(_) -> ok.

-spec format(LogEvent::logger:log_event(), Config::logger_formatter:config()) -> unicode:chardata().
format(#{level := Level, msg := Msg, meta := Meta}, Config) ->
    encode(maps:get(format, Config, ?DEFAULT_FORMAT),
           (meta(Meta))#{level => Level, node => node(), type => erlogstash,
                         message => unicode:characters_to_binary(msg(Msg, Meta))}).

%% internal functions

-spec msg(Msg::msg(), Meta::logger:metadata()) -> unicode:chardata().
msg({report, Report}, #{report_cb := Fun}) when is_function(Fun, 2) -> Fun(Report, #{single_line => true});
msg({report, Report}, #{report_cb := Fun}) when is_function(Fun, 1) ->
    {F, A} = Fun(Report),
    io_lib:format(F, A);
msg({report, Report}, _) -> io_lib:write(Report);
msg({string, D}, _) -> D;
msg({F, A}, _) -> io_lib:format(F, A).

-spec meta(Meta::logger:metadata()) -> data().
meta(Meta) -> maps:fold(fun meta/3, #{}, Meta).

-spec meta(Tag::atom(), V::term(), A::data()) -> data().
meta(time, V, A) ->
    A#{'@timestamp' => list_to_binary(calendar:system_time_to_rfc3339(V, [{unit, microsecond}, {offset, "Z"}]))};
meta(domain, V, A) -> A#{domain => V};
meta(file, V, A) -> A#{file => unicode:characters_to_binary(V)};
meta(mfa, {M, F, Arity}, A) -> A#{module => M, function => F, arity => Arity, mfa => mfa(M, F, Arity)};
meta(K, _, A) when K =:= error_logger; K =:= logger_formatter; K =:= report_cb; K =:= gl -> A;
meta(K, undefined, A) -> A#{K => null};
meta(K, P, A) when is_pid(P) -> A#{K => list_to_binary(pid_to_list(P))};
meta(K, P, A) when is_port(P) -> A#{K => list_to_binary(port_to_list(P))};
meta(K, V, A) when is_number(V); is_atom(V); is_binary(V) -> A#{K => V};
meta(K, V, A) ->
    A#{K => unicode:characters_to_binary(case io_lib:char_list(V) of
                                             true -> V;
                                             _false -> io_lib:write(V)
                                         end)}.

-spec mfa(M::module(), F::atom(), A::non_neg_integer()) -> binary().
mfa(M, F, A) ->
    <<(atom_to_binary(M, latin1))/binary, $:, (atom_to_binary(F, utf8))/binary, $/, (integer_to_binary(A))/binary>>.

-spec encode(Format::format(), Data::data()) -> iodata().
encode(json, Data) -> jsone:encode(Data);
encode(json_line, Data) -> [encode(json, Data), $\n];
encode(msgpack, Data) -> msgpack:pack(Data, [{pack_str, none}]).
