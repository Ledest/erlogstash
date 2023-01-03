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
encode(Data, json) -> enc_json(Data);
encode(Data, json_line) -> [encode(Data, json), $\n];
encode(Data, msgpack) -> enc_msgpack(Data).

enc_json(null) -> <<"null">>;
enc_json(false) -> <<"false">>;
enc_json(true) -> <<"true">>;
enc_json(V) when is_list(V) -> enc_json_list(V);
enc_json(V) when is_integer(V) -> integer_to_binary(V);
enc_json(V) when is_float(V) -> float_to_binary(V);
enc_json(V) when is_atom(V) -> enc_json_str(atom_to_binary(V));
enc_json(V) when is_binary(V) -> [$", V, $"];
enc_json(M) when is_map(M) ->
    [${, join(maps:fold(fun(K, V, A) -> [[$", json_key(K), $", $:, enc_json(V)]|A] end, [], M)), $}];
enc_json(V) when is_tuple(V) -> enc_json_list(tuple_to_list(V)).

enc_json_list(V) -> [$[, join(lists:map(fun enc_json/1, V)), $]].

enc_json_str(V) -> [$", V, $"].

json_key(K) when is_atom(K) -> atom_to_binary(K);
json_key(K) when is_binary(K) -> K;
json_key(K) when is_integer(K) -> integer_to_binary(K).

join(L) -> lists:join($,, L).

enc_msgpack(null) -> 16#C0;
enc_msgpack(false) -> 16#C2;
enc_msgpack(true) -> 16#C3;
enc_msgpack(V) when is_binary(V) -> enc_msgpack_str(V);
enc_msgpack(V) when is_list(V) -> enc_msgpack_list(V);
enc_msgpack(V) when is_float(V) -> <<16#CB, V/float>>;
enc_msgpack(V) when is_atom(V) -> enc_msgpack_str(atom_to_binary(V));
enc_msgpack(V) when is_integer(V), V >= 0 ->
    if
        V >= 0 ->
            if
                V < 1 bsl 7 -> V;
                V < 1 bsl 8 -> <<16#CC, V:1/unit:8>>;
                V < 1 bsl 16 -> <<16#CD, V:2/unit:8>>;
                V < 1 bsl 32 -> <<16#CE, V:4/unit:8>>;
                V < 1 bsl 64 -> <<16#CF, V:8/unit:8>>;
                true -> enc_msgpack_str(integer_to_binary(V))
            end;
        true ->
            if
                V >= -1 bsl 5 -> <<2#111:3, V:5>>;
                V >= -1 bsl 7 -> <<16#D0, V:1/unit:8>>;
                V >= -1 bsl 15 -> <<16#D1, V:2/unit:8>>;
                V >= -1 bsl 31 -> <<16#D2, V:4/unit:8>>;
                V >= -1 bsl 63 -> <<16#D3, V:8/unit:8>>;
                true -> enc_msgpack_str(integer_to_binary(V))
            end
    end;
enc_msgpack(M) when is_map(M) ->
    [case map_size(M) of
         S when S < 1 bsl 4 -> 16#80 bor S;
         S when S < 1 bsl 16 -> <<16#DE, S:2/unit:8>>;
         S -> <<16#DF, S:4/unit:8>>
     end|maps:fold(fun(K, V, A) -> [enc_msgpack(K), enc_msgpack(V)|A] end, [], M)];
enc_msgpack(V) when is_tuple(V) -> enc_msgpack_list(tuple_to_list(V)).

enc_msgpack_str(V) ->
    case byte_size(V) of
        S when S < 1 bsl 5 -> [2#10100000 bor S, V];
        S when S < 1 bsl 8 -> [16#D9, S, V];
        S when S < 1 bsl 16 -> [<<16#DA, S:2/unit:8>>, V];
        S -> [<<16#DB, S:4/unit:8>>, V]
    end.

enc_msgpack_list(V) ->
    case lists:mapfoldl(fun(E, C) -> {enc_msgpack(E), C + 1} end, 0, V) of
        {A, S} when S < 1 bsl 4 -> [16#90 bor S|A];
        {A, S} when S < 1 bsl 16 -> [<<16#DC, S:2/unit:8>>|A];
        {A, S} -> [<<16#DD, S:4/unit:8>>|A]
    end.
