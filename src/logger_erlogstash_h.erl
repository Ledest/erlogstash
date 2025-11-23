-module(logger_erlogstash_h).

%% logger callbacks
-export([adding_handler/1, removing_handler/1, log/2, changing_config/3]).

-define(VALID_CONFIG_KEYS, [format, timestamp, tags]).

-spec adding_handler(HConfig::logger:handler_config()) -> {ok, logger:handler_config()} | {error, term()}.
adding_handler(#{id := N, output := Output} = HConfig) ->
    case erlogstash:start_worker(N, Output) of
        {ok, _} -> {ok, config(HConfig)};
        {error, _} = E -> E
    end.

-spec removing_handler(HConfig::logger:handler_config()) -> ok.
removing_handler(#{id := N}) -> erlogstash:stop_worker(N).

-spec log(LogEvent::logger:log_event(), HConfig::logger:handler_config()) -> ok.
log(LogEvent, #{id := N, formatter := {M, FC}}) -> erlogstash:send(N, M:format(LogEvent, FC)).

-spec changing_config(set|update, OConfig::logger:handler_config(), NConfig::logger:handler_config()) ->
          {ok, logger:handler_config()} | {error, term()}.
changing_config(set, #{id := N, output := Output}, #{id := N, output := Output} = NConfig) -> {ok, config(NConfig)};
changing_config(set, #{id := N}, #{id := N} = NConfig) ->
    erlogstash:stop_worker(N),
    adding_handler(NConfig);
changing_config(set, _, NConfig) -> adding_handler(NConfig);
changing_config(update, #{id := N, output := Output} = OConfig, #{id := N, output := Output} = NConfig) ->
    {ok, config(maps:merge(OConfig, NConfig))};
changing_config(update, #{id := N} = OConfig, #{id := N} = NConfig) ->
    erlogstash:stop_worker(N),
    adding_handler(maps:merge(OConfig, NConfig));
changing_config(set, OConfig, NConfig) -> adding_handler(maps:merge(OConfig, NConfig)).

%% internal functions
-spec config(HConfig::logger:handler_config()) -> logger:handler_config().
config(#{formatter := {logger_formatter, _}} = HConfig) ->
    HConfig#{formatter := {logger_erlogstash_formatter, fconfig(HConfig, #{})}};
config(#{formatter := {M, FConfig}} = HConfig) -> HConfig#{formatter := {M, fconfig(HConfig, FConfig)}};
config(#{formatter := M} = HConfig) -> HConfig#{formatter := {M, fconfig(HConfig, #{})}};
config(HConfig) -> HConfig#{formatter => {logger_erlogstash_formatter, fconfig(HConfig, #{})}}.

-spec fconfig(HConfig::logger:handler_config(), FConfig::logger:formatter_config()) -> logger:formatter_config().
fconfig(HConfig, FConfig) ->
    maps:merge(maps:with(?VALID_CONFIG_KEYS, HConfig),
               case HConfig of
                   #{count := true} -> FConfig#{count => atomics:new(1, [{signed, false}])};
                   _ -> FConfig
               end).
