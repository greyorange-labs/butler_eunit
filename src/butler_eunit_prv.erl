-module(butler_eunit_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, eunit).
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
            {short_desc, "eunit setup for butler server"},
            {desc, "eunit setup for butler server"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    os:cmd("rm -rf Mnesia.butler_server.test"),
    application:set_env(mnesia, dir, "Mnesia.butler_server.test"),
    application:ensure_all_started(gproc),
    metric_utils:init_metrics(),
    butler_setup:initialize_all_caches(),
    order_fulfilment_sup:initialize_simple_caches(),
    butler_setup:init_database_from_model_list(models:all()),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
