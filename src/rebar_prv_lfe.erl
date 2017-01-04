-module(rebar_prv_lfe).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, _State1} = rebar_prv_lfe_compile:init(State).
