# erlogstash

Application for sending logs to [Logstash][logstash].

## Configuration

Add `erlogstash` to your `rebar.config` deps:

``` erlang
{deps, [
    {erlogstash, {git, "https://github.com/Ledest/erlogstash.git", {branch, "master"}}}
]}.
```

And finally, configure `erlogstash` app with something like this:

``` erlang
[{erlogstash, [
   {handlers,
    [
     {lager_logstash_backend,
      [
       {level, info},
       {output, {tcp, "localhost", 5000}},
       %% {output, {udp, "localhost", 5000}},
       %% {output, {file, "/var/log/lager_logstash.log"}},
       {encoder, jsx}
      ]}
    ]}
]}
].
```

## Features

  * outputs: `tcp`, `udp`, `file`
  * formats: `json`, `msgpack`

[logstash]: http://logstash.net
