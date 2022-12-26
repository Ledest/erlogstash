-module(logger_erlogstash_h).

%% logger callbacks
-export([log/2]).

-define(DEFAULT_FORMAT, json).

log(LogEvent, #{formatter := {logger_formatter, _}} = Config) ->
    log(LogEvent, Config#{formatter => {logger_erlogstash_formatter, #{}}}, logger_erlogstash_formatter, #{});
log(LogEvent, #{formatter := {M, C}} = Config) -> log(LogEvent, Config, M, C);
log(LogEvent, #{formatter := M} = Config) -> log(LogEvent, Config#{formatter => {M, #{}}}, M, #{});
log(LogEvent, Config) ->
    log(LogEvent, Config#{formatter => {logger_erlogstash_formatter, #{}}}, logger_erlogstash_formatter, #{}).

log(LogEvent, Config, Formatter, FConfig) -> erlogstash:send(erlogstash2, Formatter:format(LogEvent, FConfig)).
