[![Build Status](https://github.com/Ledest/erlogstash/actions/workflows/erlang.yml/badge.svg)](https://github.com/Ledest/erlogstash/actions/workflows/erlang.yml/badge.svg)

# erlogstash

Application for sending logs to [Logstash][logstash].

Inspired by [lager_logstash].

## Configuration

Add `erlogstash` to your `rebar.config` deps:

```erlang
{deps, [
    {erlogstash, {git, "https://github.com/Ledest/erlogstash.git", {branch, "master"}}}
]}.
```

Configure `erlogstash` app with something like this:

```erlang
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
                % json is default format
            }},
            {handler, logstash2, logger_erlogstash_h, #{
                output => {tcp, "localhost", 5001},
                tags => #{app => myapp, mytag => "my_tag_value"},
                timestamp => iso8601, % default
                format => json_lines
            }},
            {handler, logstash2, logger_erlogstash_h, #{
                output => {udp, {172,22,160,1}, 5000}
                count => true,
                tags => [{app, myapp}, {mytag, <<"MY_TAG_VALUE">>}],
                timestamp => unix_ms,
                format => msgpack
            }}
        ]}
    ]}
].
```

## Features

  * outputs: `tcp`, `udp`, `file`
  * formats: `json`, `json_lines`, `msgpack`

## Warning

Logstash TCP output is quiet stupid and ugly.
So it's a bad idea to use `tcp` output with a format other than `json_lines`.

[logstash]: https://www.elastic.co/logstash/
[lager_logstash]: https://github.com/rpt/lager_logstash.git
