-module(logger_erlogstash_formatter).

%% formatter callbacks
-export([check_config/1, format/2]).

-define(DEFAULT_FORMAT, json).
-define(DEFAULT_TIMESTAMP, iso8601).
-define(DEFAULT_CONFIG, #{timestamp => ?DEFAULT_TIMESTAMP, format => ?DEFAULT_FORMAT}).

-define(VALID_FORMAT, [json, json_lines, msgpack]).
-define(VALID_TIMESTAMP, [iso8601, unix_ms, unix]).

-type format() :: json | json_lines | msgpack.
-type timestamp_format() :: iso8601 | unix_ms | unix.
-type value() :: null | boolean() | atom() | binary() | number() | [value()] | #{atom() => value()}.
-type data() :: #{atom() => value()}.
-type msg() :: {io:format(), [term()]} | {report, logger:report()} | {string, unicode:chardata()}.

-spec check_config(Config::logger_formatter:config()) -> ok | {error, term()}.
check_config(Config) -> check_config(Config, [{format, ?VALID_FORMAT}, {timestamp, ?VALID_TIMESTAMP}]).

-spec format(LogEvent::logger:log_event(), Config::logger_formatter:config()) -> unicode:chardata().
format(#{level := Level, msg := Msg, meta := #{time := T} = Meta}, Config) ->
    #{timestamp := Timestamp, format := Format} = maps:merge(?DEFAULT_CONFIG, Config),
    M = add_count(meta(add_tags(Meta, Config)), Config),
    encode(M#{level => Level, type => erlogstash, '@timestamp' => timestamp(T, Timestamp), node => node(),
              message => unicode:characters_to_binary(msg(Msg, Meta))},
           Format).

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

-spec add_count(M::map(), logger_formatter:config()) -> map().
add_count(M, #{count := R}) when is_reference(R) ->
    try atomics:add_get(R, 1, 1) of
        C -> M#{count => C}
    catch
        _:_ -> M
    end;
add_count(M, _) -> M.

-spec add_tags(M::map(), logger_formatter:config()) -> map().
add_tags(M, #{tags := T}) -> add_tags_(M, T);
add_tags(M, _) -> M.

-spec add_tags_(M::map(), map()|[{_, _}]) -> map().
add_tags_(M, T) when is_map(T) -> maps:merge(M, T);
add_tags_(M, T) when is_list(T) ->
    lists:foldl(fun({K, V}, A) -> A#{K => V};
                   (K, A) -> A#{K => true}
                end, M, T);
add_tags_(M, _) -> M.

-spec msg(Msg::msg(), Meta::logger:metadata()) -> unicode:chardata().
msg({report, Report}, #{report_cb := Fun}) when is_function(Fun, 2) -> Fun(Report, #{single_line => true});
msg({report, Report}, #{report_cb := Fun}) when is_function(Fun, 1) ->
    {F, A} = Fun(Report),
    io_lib:format(F, A);
msg({report, Report}, _) ->
    {F, A} = logger:format_report(Report),
    string:trim(re:replace(io_lib:format(F, A), <<",?\r?\n *">>, ", ", [global, unicode]), leading);
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
value(V) when is_atom(V); is_binary(V); is_number(V) -> V;
value(P) when is_pid(P) -> list_to_binary(pid_to_list(P));
value(P) when is_port(P) -> list_to_binary(port_to_list(P));
value(R) when is_reference(R) -> list_to_binary(ref_to_list(R));
value(M) when is_map(M) -> maps:map(fun(_, V) -> value(V) end, M);
value(L) when is_list(L) ->
    case io_lib:printable_unicode_list(L) of
        true -> unicode:characters_to_binary(L);
        _false -> lists:map(fun value/1, L)
    end;
value(T) when is_tuple(T) -> lists:map(fun value/1, tuple_to_list(T));
value(F) when is_function(F) ->
    case maps:from_list(erlang:fun_info(F)) of
        #{type := external} -> unicode:characters_to_binary(erlang:fun_to_list(F));
        #{module := M, name := N, arity := A} -> <<"fun ", (mfa(M, N, A))/binary>>
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

-compile({parse_transform, otpbp_pt}).

-spec encode(Data::data(), Format::format()) -> iodata().
encode(Data, json) -> json:encode(Data);
encode(Data, json_lines) -> [json:encode(Data), $\n];
encode(Data, msgpack) -> emsgpack:encode(Data).
