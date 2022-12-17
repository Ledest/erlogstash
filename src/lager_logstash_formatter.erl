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

format(Msg, #{encoder := Encoder, tag := undefined}) -> format(Msg, Encoder, []);
format(Msg, #{encoder := Encoder, tag := Tags}) -> format(Msg, Encoder, Tags).

format(Msg, Encoder, Tags) ->
    encode(Encoder,
           convert(Tags ++
                   [{type, lager_logstash},
                    {level, lager_msg:severity(Msg)},
                    {'@timestamp', timestamp(lager_msg:timestamp(Msg))},
                    {message, lager_msg:message(Msg)}] ++
                   convert_metadata(lager_msg:metadata(Msg)))).

timestamp(U) when is_integer(U) -> calendar:system_time_to_rfc3339(U, [{unit, microsecond}, {offset, "Z"}]);
timestamp({M, S, U}) -> timestamp(M * (1000000 * 1000000) + S * 1000000 + U).

convert_metadata(L) ->
    lists:map(fun({Key, Value}) when is_tuple(Value) -> {Key, unicode:characters_to_binary(io_lib:write(Value))};
                 (M) -> M
              end, L).

convert(Data) ->
    lists:filtermap(fun({_, undefined}) -> false;
                       ({K, V}) when is_atom(V) -> {true, {K, atom_to_binary(V, latin1)}};
                       ({K, Pid}) when K =:= pid orelse K =:= name, is_pid(Pid) ->
                        {true, {K, list_to_binary(pid_to_list(Pid))}};
                       ({K, V}) when is_list(V) -> {true, {K, unicode:characters_to_binary(V)}};
                       (_) -> true
                    end, Data).

encode(J, Data) when J =:= jsx; J =:= jsone -> [J:encode(Data), $\n];
encode(jiffy, Data) -> [jiffy:encode({Data}), $\n];
encode(msgpack, Data) -> msgpack:pack(Data, [{pack_str, none}, {map_format, jsx}]).
