-module(rebar_prv_lfe_compile).

%% Public API.
-export([init/1, do/1, format_error/1]).

%% Macros.
-define(PROVIDER, compile).
-define(NAMESPACE, lfe).
-define(DEPS, [{default, app_discovery}]).
-define(LFE_OPTS, lfe_opts).

%% Bloody useful
-define(IF(Test,True,False), case Test of true -> True; false -> False end).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create(
                 [
                  {name, ?PROVIDER},               % The 'user friendly' name of the task
                  {namespace, ?NAMESPACE},
                  {module, ?MODULE},               % The module implementation of the task
                  {bare, true},                    % The task can be run by the user, always true
                  {deps, ?DEPS},                   % The list of dependencies
                  {example, "rebar3 lfe compile"}, % How to use the plugin
                  {opts, []},                      % list of options understood by the plugin
                  {short_desc, "LFE rebar3 compiler plugin"},
                  {desc, ""}
                 ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = case rebar_state:current_app(State) of
               undefined ->
                   rebar_state:project_apps(State);
               AppInfo ->
                   [AppInfo]
           end,
    DepsDir = rebar_dir:deps_dir(State),
    [compile(App, DepsDir) || App <- Apps],
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Private Parts
%% ===================================================================

%% Modified from rebar_prv_alpaca:do/1 and lr3_comp:compile_dir/4.
-spec compile(rebar_app_info:t(), file:dirname()) -> ok.
compile(AppInfo, DepsDir) ->
    AppDir = rebar_app_info:dir(AppInfo),
    SourceDir = filename:join(AppDir, "src"),
    EBinDir = rebar_app_info:ebin_dir(AppInfo),
    Opts = rebar_app_info:opts(AppInfo),
    Config = config(DepsDir, EBinDir, Opts),
    FirstFiles = lfe_first_files(Opts, AppDir),
    compile_dir(SourceDir, EBinDir, Config, FirstFiles),
    ExtraDirs = rebar_dir:extra_src_dirs(Opts),
    [compile_dir(Dir, EBinDir, Config, FirstFiles) || Dir <- ExtraDirs],
    ok.

compile_dir(SourceDir, EBinDir, Config, FirstFiles) ->
    rebar_base_compiler:run(Config, FirstFiles,
                            SourceDir, ".lfe",
                            EBinDir, ".beam",
                            fun compile_lfe/3).

%% Modified from lr3_comp:compile/3.
-spec compile_lfe(Source, Target, Config) -> Result when
      Source :: file:filename(),
      Target :: file:filename(),
      Config :: dict:dict(),
      Result :: ok | {ok, [string()]} | rebar_base_compiler:error_tuple().
compile_lfe(Source, Target, Config) ->
    LfeOpts = dict:fetch(lfe_opts, Config),

    rebar_api:debug("Compiling ~s to ~s with opts: ~p",
                    [Source, Target, LfeOpts]),

    case lfe_comp:file(Source, LfeOpts) of
        {ok, _Mod} ->
            ok;
        {ok, _Mod, Ws} ->
            rebar_base_compiler:ok_tuple(Source, Ws);
        {error, [], Es, Ws} ->
            rebar_base_compiler:error_tuple(Source, Es, Ws, Config);
        {error, [{error, Es, Ws}|_], _Es, _Ws} ->
            rebar_base_compiler:error_tuple(Source, Es, Ws, Config)
    end.

%% Modified from lr3_comp_util:config/2.
-spec config(DepsDir, EBinDir, Config1) -> Config2 when
      DepsDir  :: file:dirname(),
      EBinDir  :: file:dirname(),
      Config1  :: [proplists:property()] | dict:dict(),
      Config2  :: [proplists:property()] | dict:dict().
config(DepsDir, EBinDir, Config) ->
    %% TODO: clean this up
    Opts = [ {outdir, EBinDir}, {i, DepsDir}, {i, "include"}, return, verbose
             | rebar_opts:erl_opts(Config) ],
    ?IF(dict:is_key(?LFE_OPTS, Config),
        dict:append_list(?LFE_OPTS, Opts, Config),
        dict:store(?LFE_OPTS, Opts, Config)).

%% Renamed lr3_comp_util:get_first_files/2.
-spec lfe_first_files(Opts, AppDir) -> Files when
      Opts   :: [proplists:property()] | dict:dict(),
      AppDir :: file:dirname(),
      Files  :: [file:filename()].
lfe_first_files(Opts, AppDir) ->
    Dirs = rebar_opts:get(Opts, lfe_first_files, []),
    [filename:join(AppDir, Dir) || Dir <- Dirs].
