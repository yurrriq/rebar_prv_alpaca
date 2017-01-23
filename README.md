# rebar_prv_lfe

[![Build Status][Travis badge]][Travis link]
[![Coverage Status][Coveralls badge]][Coveralls link]

*A `rebar3` plugin for compiling [LFE][] modules.*

## Usage

Add the plugin to your `rebar.config`:

```erlang
{plugins, [
  {rebar_prv_lfe,
   {git, "git://github.com/yurrriq/rebar_prv_lfe.git",
    {tag, "0.2.0"}}}
]}.


{provider_hooks, [{pre, [{app_compile, {lfe, compile}}]}]}.
```


```
$ rebar3 shell
===> Compiling rebar_prv_lfe
===> Compiling rebar_prv_lfe
===> Compiling rebar_prv_lfe
===> Verifying dependencies...
===> Compiling elli
===> Compiling lfe-test
Erlang/OTP 19 [erts-8.2] [source] [64-bit] [smp:8:8] [async-threads:0] [hipe] [kernel-poll:false] [dtrace]

Eshell V8.2  (abort with ^G)
1> r3:do(compile).
===> This feature is experimental and may be modified or removed at any time.
Compiling rebar_prv_lfe
Compiling rebar_prv_lfe
Compiling rebar_prv_lfe
Verifying dependencies...
Compiling lfe-test
ok
2>
```


<!-- Named Links -->

[Travis badge]: https://travis-ci.org/yurrriq/rebar_prv_lfe.svg?branch=develop
[Travis link]: https://travis-ci.org/yurrriq/rebar_prv_lfe
[Coveralls badge]: https://coveralls.io/repos/github/yurrriq/rebar_prv_lfe/badge.svg?branch=develop
[Coveralls link]: https://coveralls.io/github/yurrriq/rebar_prv_lfe?branch=develop
[LFE]: https://github.com/rvirding/lfe
