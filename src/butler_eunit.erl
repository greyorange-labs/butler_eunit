-module(butler_eunit).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = butler_eunit_prv:init(State),
    {ok, State1}.
