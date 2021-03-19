# redis

`redis` is an application and library implementing redis protocol in
Erlang.

## Usage

```sh
rebar3 shell
```

### Serializer

Encoding Erlang terms in Redis data format using `redis:encode/1`
function:

```erlang
% integer
redis:encode(1).

% simple string
redis:encode(<<"test">>).

% bulk string
redis:encode({bulk_string, <<"test">>).

% array
redis:encode([1,2,3,<<"test">>, {bulk_string, <<"test">>}]).

% error
redis:encode({error, <<"my message">>}).
```

Decoding Redis data in Erlang term with `redis:decode/1` function:

```erlang
% simple string
redis:decode(<<"+OK\r\n">>).

% integer
redis:decode(<<":1\r\n">>).

% bulk string
redis:decode(<<"$3\r\nfoo\r\n">>).

% array
redis:decode(<<"*0\r\n\r\n">>).

% error
redis:decode(<<"-Message\r\n">>).
```

### Client

wip.

### Server

wip.

## Test

```sh
rebar3 eunit
```

# Resources and References

 * https://redis.io/topics/protocol

# About

Made with <3 by Mathieu Kerjouan with [Erlang](erlang.org/) and
[rebar3](https://www.rebar3.org).
