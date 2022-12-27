-module(logger_erlogstash_formatter).

%% formatter callbacks
-export([check_config/1, format/2]).

-define(DEFAULT_FORMAT, json).

-type format() :: json|msgpack.
-type data() :: [{atom(), null|atom()|number()|binary()|[atom()]}].
-type msg() :: {io:format(), [term()]} | {report, logger:report()} | {string, unicode:chardata()}.

-spec check_config(logger_formatter:config()) -> ok | {error, term()}.
check_config(#{format := F}) when F =/= json, F =/= msgpack -> {error, {format, F}};
check_config(_) -> ok.

-spec format(logger:log_event(), Config::logger_formatter:config()) -> unicode:chardata().
format(#{level := Level, msg := Msg, meta := Meta}, Config) ->
    encode(maps:get(format, Config, ?DEFAULT_FORMAT),
           [{level, Level}, {node, node()}, {message, unicode:characters_to_binary(msg(Msg, Meta))}|meta(Meta)]).

%% internal functions

-spec msg(msg(), logger:metadata()) -> unicode:chardata().
msg({report, Report}, #{report_cb := Fun}) when is_function(Fun, 2) -> Fun(Report, #{single_line => true});
msg({report, Report}, #{report_cb := Fun}) when is_function(Fun, 1) ->
    {F, A} = Fun(Report),
    io_lib:format(F, A);
msg({report, Report}, _) -> io_lib:write(Report);
msg({string, D}, _) -> D;
msg({F, A}, _) -> io_lib:format(F, A).

-spec meta(Meta::logger:metadata()) -> data().
meta(Meta) -> maps:fold(fun meta/3, [], Meta).

-spec meta(atom(), term(), A::data()) -> data().
meta(time, V, A) -> 
    [{'@timestamp', list_to_binary(calendar:system_time_to_rfc3339(V, [{unit, microsecond}, {offset, "Z"}]))}|A];
meta(domain, V, A) -> [{domain, V}|A];
meta(file, V, A) -> [{file, unicode:characters_to_binary(V)}|A];
meta(mfa, {M, F, Arity}, A) -> [{module, M}, {function, F}, {arity, Arity}, {mfa, mfa(M, F, Arity)}|A];
meta(K, _, A) when K =:= error_logger; K =:= logger_formatter; K =:= report_cb; K =:= gl -> A;
meta(K, undefined, A) -> [{K, null}|A];
meta(K, P, A) when is_pid(P) -> [{K, list_to_binary(pid_to_list(P))}|A];
meta(K, V, A) when is_number(V); is_atom(V); is_binary(V) -> [{K, V}|A];
meta(K, V, A) when is_list(V) ->
    [{K, unicode:characters_to_binary(case io_lib:char_list(V) of
                                          true -> V;
                                          _false -> io_lib:write(V)
                                      end)}|A];
meta(K, V, A) -> [{K, unicode:characters_to_binary(io_lib:write(V))}|A].

-spec mfa(module(), atom(), non_neg_integer()) -> binary().
mfa(M, F, A) ->
    <<(atom_to_binary(M, latin1))/binary, $:, (atom_to_binary(F, utf8))/binary, $/, (integer_to_binary(A))/binary>>.

-spec encode(format(), Data::data()) -> iodata().
encode(json, Data) -> [jsone:encode(Data), $\n];
encode(msgpack, Data) -> msgpack:pack(Data, [{pack_str, none}, {map_format, jsx}]).
