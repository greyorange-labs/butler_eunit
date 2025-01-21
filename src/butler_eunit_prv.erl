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
    io:format("Eunit: Pre setup [start]......~n"),
    os:cmd("rm -rf Mnesia.butler_server.test"),
    application:set_env(mnesia, dir, "Mnesia.butler_server.test"),
    application:ensure_all_started(gproc),
    metric_utils:init_metrics(),
    butler_setup:initialize_all_caches(),
    order_fulfilment_sup:initialize_simple_caches(),
    %% 1. Runs old migrations
    io:format("Running Base migrations..........~n"),
    db_setup:init_databases([]),
    Apps = application:get_env(butler_server, x_runtime_apps, [gmc, non_gmc]),
    application:set_env(mnesia_migrate, verbose, false),
    application:set_env(erl_migrate, verbose, false),
    %% 2. Runs `GMC` migrations
    io:format("Running GMC migrations..........~n"),
    case lists:member(gmc, Apps) of
        true -> ok = gmc_db_setup:init_migrations();
        false -> ok
    end,
    %% 3. Runs `GMR` migrations
    io:format("Running GMR migrations..........~n"),
    case lists:member(non_gmc, Apps) of
        true -> ok = gmr_db_setup:init_migrations();
        false -> ok
    end,
    bsh_global_data:ensure_advance_logging_record(),
    bsh_sysmon:init_cache(),
    io:format("Eunit: Pre setup [complete]......~n"),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
