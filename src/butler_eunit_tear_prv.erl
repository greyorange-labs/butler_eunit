-module(butler_eunit_tear_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, eunit_tear).
-define(NAMESPACE, butler_eunit).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {namespace, ?NAMESPACE},
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 butler_eunit"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "eunit teardown for butler server"},
            {desc, "eunit teardown for butler server"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    application:stop(mnesia),
    application:stop(boss_db),
    application:stop(gproc),
    os:cmd("rm -rf Mnesia.butler_server.test"),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
