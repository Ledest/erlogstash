-module(logger_erlogstash_formatter).

%% formatter callbacks
-export([format/2]).

-define(DEFAULT_FORMAT, json).

format(#{level := Level, msg := Msg, meta := Meta}, Config) ->
    encode(maps:get(format, Config, ?DEFAULT_FORMAT),
           [{level, Level}, {message, unicode:characters_to_binary(msg(Msg, Meta))}|meta(Meta)]).

msg({report, Report}, #{report_cb := Fun}) when is_function(Fun, 2) -> Fun(Report, #{single_line => true});
msg({report, Report}, #{report_cb := Fun}) when is_function(Fun, 1) ->
    {F, A} = Fun(Report),
    io_lib:format(F, A);
msg({report, Report}, _) -> io_lib:write(Report);
msg({string, D}, _) -> D;
msg({F, A}, _) -> io_lib:format(F, A).

meta(Meta) -> maps:fold(fun meta/3, [], Meta).

meta(time, V, A) ->
    [{'@timestamp', list_to_binary(calendar:system_time_to_rfc3339(V, [{unit, microsecond}, {offset, "Z"}]))}|A];
meta(domain, V, A) -> [{domain, V}|A];
meta(file, V, A) -> [{file, unicode:characters_to_binary(V)}|A];
meta(mfa, {M, F, Arity}, A) -> [{module, M}, {function, F}, {arity, Arity}|A];
meta(K, _, A) when K =:= error_logger; K =:= logger_formatter; K =:= report_cb -> A;
meta(K, undefined, A) -> [{K, null}|A];
meta(K, P, A) when is_pid(P) -> [{K, list_to_binary(pid_to_list(P))}|A];
meta(K, V, A) when is_number(V); is_atom(V); is_binary(V) -> [{K, V}|A];
meta(K, V, A) when is_list(V) ->
    [{K, unicode:characters_to_binary(case io_lib:char_list(V) of
                                          true -> V;
                                          _false -> io_lib:write(V)
                                      end)}|A];
meta(K, V, A) -> [{K, unicode:characters_to_binary(io_lib:write(V))}|A].

encode(json, Data) -> [jsone:encode(Data), $\n];
encode(msgpack, Data) -> msgpack:pack(Data, [{pack_str, none}, {map_format, jsx}]).
