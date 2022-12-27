-module(logger_erlogstash_h).

%% logger callbacks
-export([adding_handler/1, removing_handler/1, log/2]).

adding_handler(#{id := N, output := Output} = Config) ->
    case erlogstash:start_worker(N, Output) of
        {ok, _} -> {ok, Config};
        {error, _} = E -> E
    end.

removing_handler(#{worker := P}) -> gen_server:cast(P, stop).

log(LogEvent, #{formatter := {logger_formatter, _}} = Config) ->
    FC = config(#{}, Config),
    log(LogEvent, Config#{formatter => {logger_erlogstash_formatter, FC}}, logger_erlogstash_formatter, FC);
log(LogEvent, #{formatter := {M, C}} = Config) -> log(LogEvent, Config, M, config(C, Config));
log(LogEvent, #{formatter := M} = Config) ->
    FC = config(#{}, Config),
    log(LogEvent, Config#{formatter => {M, FC}}, M, FC);
log(LogEvent, Config) ->
    FC = config(#{}, Config),
    log(LogEvent, Config#{formatter => {logger_erlogstash_formatter, FC}}, logger_erlogstash_formatter, FC).

log(LogEvent, #{id := N}, Formatter, FConfig) -> erlogstash:send(N, Formatter:format(LogEvent, FConfig)).

config(#{format := _} = Config, _) -> Config;
config(Config, #{format := F}) -> Config#{format => F};
config(Config, _) -> Config.
