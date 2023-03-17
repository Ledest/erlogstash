-module(logger_erlogstash_h).

%% logger callbacks
-export([adding_handler/1, removing_handler/1, log/2]).

-define(VALID_CONFIG_KEYS, [format, timestamp, tags]).

-spec adding_handler(HConfig::logger:handler_config()) -> {ok, logger:handler_config()} | {error, term()}.
adding_handler(#{id := N, output := Output} = HConfig) ->
    case erlogstash:start_worker(N, Output) of
        {ok, _} -> {ok, config(HConfig)};
        {error, _} = E -> E
    end.

-spec removing_handler(HConfig::logger:handler_config()) -> ok.
removing_handler(#{id := N}) -> erlogstash:stop_worker(N).

-spec log(LogEvent::logger:log_event(), HConfig::logger:handler_config()) -> any().
log(LogEvent, #{id := N, formatter := {M, FC}}) -> erlogstash:send(N, M:format(LogEvent, FC)).

%% internal functions
-spec config(HConfig::logger:handler_config()) -> logger:handler_config().
config(#{formatter := {logger_formatter, _}} = HConfig) ->
    HConfig#{formatter := {logger_erlogstash_formatter, fconfig(#{}, HConfig)}};
config(#{formatter := {M, FConfig}} = HConfig) -> HConfig#{formatter := {M, fconfig(FConfig, HConfig)}};
config(#{formatter := M} = HConfig) -> HConfig#{formatter := {M, fconfig(#{}, HConfig)}};
config(HConfig) -> HConfig#{formatter => {logger_erlogstash_formatter, fconfig(#{}, HConfig)}}.

-spec fconfig(FConfig::logger:formatter_config(), HConfig::logger:handler_config()) -> logger:formatter_config().
fconfig(FConfig, HConfig) -> maps:merge(maps:with(?VALID_CONFIG_KEYS, HConfig), FConfig).
