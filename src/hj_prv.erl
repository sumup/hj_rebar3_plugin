-module(hj_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, compile).
-define(DEPS, [compile, app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create(
                 [{name, ?PROVIDER},                    % The 'user friendly' name of the task
                  {namespace, hj},
                  {module, ?MODULE},                    % The module implementation of the task
                  {bare, true},                         % The task can be run by the user, always true
                  {deps, ?DEPS},                        % The list of dependencies
                  {example, "rebar3 hj "},              % How to use the plugin
                  {opts, []},                           % list of options understood by the plugin
                  {short_desc, "example rebar3 plugin"},
                  {desc, "Use this plugin with rebar3 to compile your hello_jesse json schemas"}
                 ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:info("Runnnig hj plugin...", []),
    Apps = case rebar_state:current_app(State) of
               undefined ->
                   rebar_state:project_apps(State);
               AppInfo ->
                   [AppInfo]
           end,
    lists:foreach(fun(AppInfo) ->
                          Opts  = rebar_app_info:opts(AppInfo),
                          OutDir = rebar_app_info:ebin_dir(AppInfo),
                          SchemaDir = filename:join(rebar_app_info:dir(AppInfo), "src/schema"),
                          SchemaFiles = rebar_utils:find_files(SchemaDir, ".*\\.schema\$"),
                          rebar_api:debug("Will compile following schems ~p", [SchemaFiles]),
                          CompileFun = fun(SchemaFile, Opts1) ->
                                               compile(Opts1, SchemaFile, OutDir)
                                       end,
                          rebar_base_compiler:run(Opts, [], SchemaFiles, CompileFun)
                  end, Apps),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Private
%% ===================================================================
compile(_Opts, SchemaFile, OutDir) ->
    rebar_api:debug("Compiling schema ~p to ~p", [SchemaFile, OutDir]),
    hello_jesse:compile(SchemaFile, [{outdir, OutDir}]).
