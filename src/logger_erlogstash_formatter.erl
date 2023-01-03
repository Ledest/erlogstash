-module(logger_erlogstash_formatter).

%% formatter callbacks
-export([check_config/1, format/2]).

-define(DEFAULT_FORMAT, json).
-define(DEFAULT_TIMESTAMP, iso8601).

-define(VALID_FORMAT, [json, json_line, msgpack]).
-define(VALID_TIMESRAMP, [iso8601, unix_ms, unix]).

-type format() :: json | json_line | msgpack.
-type timestamp_format() :: iso8601 | unix_ms | unix.
-type value() :: null | boolean() | atom() | binary() | number() | [value()] | #{atom() => value()}.
-type data() :: #{atom() => value()}.
-type msg() :: {io:format(), [term()]} | {report, logger:report()} | {string, unicode:chardata()}.

-spec check_config(Config::logger_formatter:config()) -> ok | {error, term()}.
check_config(Config) -> check_config(Config, [{format, ?VALID_FORMAT}, {timestamp, ?VALID_TIMESRAMP}]).

-spec format(LogEvent::logger:log_event(), Config::logger_formatter:config()) -> unicode:chardata().
format(#{level := Level, msg := Msg, meta := #{time := T} = Meta}, Config) ->
    encode((meta(Meta))#{level => Level,
                         type => erlogstash,
                         '@timestamp' => timestamp(T, maps:get(timestamp, Config, ?DEFAULT_TIMESTAMP)),
                         node => node(),
                         message => unicode:characters_to_binary(msg(Msg, Meta))},
           maps:get(format, Config, ?DEFAULT_FORMAT)).

%% internal functions

-spec check_config(Config::logger_formatter:config(), [{atom(), list()}]) -> ok | {error, term()}.
check_config(Config, [{K, L}|T]) ->
    case Config of
        #{K := V} ->
            case lists:member(V, L) of
                true -> check_config(Config, T);
                _false -> {error, {K, V}}
            end;
        _ -> check_config(Config, T)
    end;
check_config(_, []) -> ok.

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
meta(file, V, A) -> A#{file => unicode:characters_to_binary(V)};
meta(mfa, {M, F, Arity}, A) -> A#{module => M, function => F, arity => Arity, mfa => mfa(M, F, Arity)};
meta(K, _, A) when K =:= time; K =:= error_logger; K =:= logger_formatter; K =:= report_cb; K =:= gl -> A;
meta(K, V, A) -> A#{K => value(V)}.

-spec value(term()) -> value().
value(undefined) -> null;
value(V) when is_number(V); is_boolean(V); is_atom(V); is_binary(V) -> V;
value(P) when is_pid(P) -> list_to_binary(pid_to_list(P));
value(P) when is_port(P) -> list_to_binary(port_to_list(P));
value(R) when is_reference(R) -> list_to_binary(ref_to_list(R));
value(M) when is_map(M) -> maps:map(fun(_, V) -> value(V) end, M);
value(L) when is_list(L) ->
    case io_lib:char_list(L) of
        true -> unicode:characters_to_binary(L);
        _false -> lists:map(fun value/1, L)
    end;
value(T) when is_tuple(T) -> lists:map(fun value/1, tuple_to_list(T));
value(F) when is_function(F) ->
    I = erlang:fun_info(F),
    case lists:keyfind(type, 1, I) of
        {_, external} -> unicode:characters_to_binary(erlang:fun_to_list(F));
        _ ->
            {_, M} = lists:keyfind(module, 1, I),
            {_, N} = lists:keyfind(name, 1, I),
            {_, A} = lists:keyfind(arity, 1, I),
            <<"fun ", (mfa(M, N, A))/binary>>
    end.

-spec mfa(M::module(), F::atom(), A::non_neg_integer()) -> binary().
mfa(M, F, A) ->
    <<(atom_to_binary(M, latin1))/binary, $:, (atom_to_binary(F, utf8))/binary, $/, (integer_to_binary(A))/binary>>.

-spec timestamp(T::pos_integer(), Format::timestamp_format()) -> binary() | pos_integer().
timestamp(T, unix_ms) -> erlang:convert_time_unit(T, microsecond, millisecond);
timestamp(T, unix) -> erlang:convert_time_unit(T, microsecond, second);
timestamp(T, iso8601) ->
    list_to_binary(calendar:system_time_to_rfc3339(timestamp(T, unix_ms), [{unit, millisecond}, {offset, "Z"}]));
timestamp(T, _) -> T.

-spec encode(Data::data(), Format::format()) -> iodata().
encode(Data, json) -> jsone:encode(Data);
encode(Data, json_line) -> [encode(json, Data), $\n];
encode(Data, msgpack) -> msgpack:pack(Data, [{pack_str, none}]).
