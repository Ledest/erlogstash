-module(logger_erlogstash_h).

%% logger callbacks
-export([adding_handler/1, log/2]).

-define(DEFAULT_FORMAT, json).

adding_handler(#{output := Output} = Config) ->
    case erlogstash_worker:start(Output) of
        {ok, P} -> {ok, Config#{worker => P}};
        {error, _} = E -> E
    end.

log(LogEvent, #{formatter := {logger_formatter, _}} = Config) ->
    log(LogEvent, Config#{formatter => {logger_erlogstash_formatter, #{}}}, logger_erlogstash_formatter, #{});
log(LogEvent, #{formatter := {M, C}} = Config) -> log(LogEvent, Config, M, C);
log(LogEvent, #{formatter := M} = Config) -> log(LogEvent, Config#{formatter => {M, #{}}}, M, #{});
log(LogEvent, Config) ->
    log(LogEvent, Config#{formatter => {logger_erlogstash_formatter, #{}}}, logger_erlogstash_formatter, #{}).

log(LogEvent, #{worker := P}, Formatter, FConfig) -> erlogstash:send(P, Formatter:format(LogEvent, FConfig)).
