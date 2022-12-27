# erlogstash

Application for sending logs to [Logstash][logstash].

## Configuration

Add `erlogstash` to your `rebar.config` deps:

``` erlang
{deps, [
    {erlogstash, {git, "https://github.com/Ledest/erlogstash.git", {branch, "master"}}}
]}.
```

Configure `erlogstash` app with something like this:

``` erlang
[
    {erlogstash, [
        {outputs, [
            {erlogstash1, {tcp, {172,22,160,38}, 5000}},
            {erlogstash2, {file, "erlogstash2.log"}}
        ]}
    ]}
].
```

Or/and configure `logger`:

```erlang
[
    {erlogstash, [
        {logger, [
            {handler, logstash1, logger_erlogstash_h, #{
                output => {file, "erlogstash.log"},
                format => json
            }}
        ]}
    ]}
].
```

## Features

  * outputs: `tcp`, `udp`, `file`
  * formats: `json`, `msgpack`

[logstash]: http://logstash.net
