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

%% @author Krzysztof Rutka <krzysztof.rutka@gmail.com>
-module(lager_logstash_formatter).

-export([format/2]).

format(LagerMsg, #{encoder := Encoder, tag := T}) ->
    Level = lager_msg:severity(LagerMsg),
    Timestamp = timestamp(lager_msg:timestamp(LagerMsg)),
    Message = lager_msg:message(LagerMsg),
    Metadata = lager_msg:metadata(LagerMsg),
    Tags = case T of
       undefined -> [];
       List -> List
    end,
    Data = [
        {type, lager_logstash},
        {level, Level},
        {'@timestamp', Timestamp},
        {message, Message} | convert_metadata(Metadata)],
    encode(Encoder, convert(Tags ++ Data)).

timestamp(U) when is_integer(U) -> calendar:system_time_to_rfc3339(U, [{unit, microsecond}, {offset, "Z"}]);
timestamp({M, S, U}) -> timestamp(M * (1000000 * 1000000) + S * 1000000 + U).

convert_metadata(L) ->
    [do_convert_metadata(M) || M <- L].

do_convert_metadata({Key, Value}) when is_tuple(Value) ->
    {Key, unicode:characters_to_binary(io_lib:write(Value))};
do_convert_metadata(M) -> M.

convert(Data) -> lists:foldl(fun convert/2, [], Data).

convert({_, undefined}, Acc) -> Acc;
convert({pid, Pid}, Acc) when is_pid(Pid) ->
    [{pid, list_to_binary(pid_to_list(Pid))} | Acc];
convert({name, Pid}, Acc) when is_pid(Pid) ->
    [{name, list_to_binary(pid_to_list(Pid))} | Acc];
convert({K, List}, Acc) when is_list(List) ->
    [{K, unicode:characters_to_binary(List)} | Acc];
convert({K, Atom}, Acc) when is_atom(Atom) ->
    [{K, atom_to_binary(Atom, latin1)} | Acc];
convert(Else, Acc) -> [Else | Acc].

encode(J, Data) when J =:= jsx; J =:= jsone -> [J:encode(Data), $\n];
encode(jiffy, Data) -> [jiffy:encode({Data}), $\n];
encode(msgpack, Data) -> msgpack:pack(Data, [{pack_str, none}, {map_format, jsx}]).
