-module(logger_erlogstash_h).

%% logger callbacks
-export([adding_handler/1, removing_handler/1, log/2]).

adding_handler(#{id := N, output := Output} = Config) ->
    case erlogstash:start_worker(N, Output) of
        {ok, _} -> {ok, config(Config)};
        {error, _} = E -> E
    end.

removing_handler(#{id := N}) -> erlogstash:stop_worker(N).

log(LogEvent, #{id := N, formatter := {M, FC}}) -> erlogstash:send(N, M:format(LogEvent, FC)).

config(#{formatter := {logger_formatter, _}} = Config) ->
    Config#{formatter := {logger_erlogstash_formatter, fconfig(#{}, Config)}};
config(#{formatter := {M, FConfig}} = Config) -> Config#{formatter := {M, fconfig(FConfig, Config)}};
config(#{formatter := M} = Config) -> Config#{formatter := {M, fconfig(#{}, Config)}};
config(Config) -> Config#{formatter => {logger_erlogstash_formatter, fconfig(#{}, Config)}}.

fconfig(#{format := _} = FConfig, _Config) -> FConfig;
fconfig(FConfig, #{format := F} = _Config) -> FConfig#{format => F};
fconfig(FConfig, _Config) -> FConfig.
