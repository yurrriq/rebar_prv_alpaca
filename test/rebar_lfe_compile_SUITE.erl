%% NOTE: This module was originally called rebar_compile_SUITE in rebar3
%% and has been modified for rebar_prv_lfe.

-module(rebar_lfe_compile_SUITE).

-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         init_per_group/2,
         end_per_group/2,
         all/0,
         groups/0,
         build_basic_lfe_app/1, paths_basic_lfe_app/1, clean_basic_lfe_app/1]).

-import(rebar_prv_lfe_test_utils, [init_rebar_state/1, init_rebar_state/2,
                                   create_config/2,
                                   create_random_name/1, create_random_vsn/0,
                                   create_lfe_app/4,
                                   run_and_check/4]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

%% Macros.
-define(LFE_APP_CONFIG,
        [{provider_hooks, [{pre, [{app_compile, {lfe, compile}}]}]}]).

%%% ================================================== [ Common Test Callbacks ]

suite() ->
    [].

all() ->
    [
     {group, basic_lfe_app}
    ].

groups() ->
    [
     {basic_lfe_app, [],
      [build_basic_lfe_app, paths_basic_lfe_app, clean_basic_lfe_app]}
    ].

init_per_group(basic_lfe_app, Config) ->
    NewConfig = init_rebar_state(Config, "basic_lfe_app_"),
    AppDir = ?config(apps, NewConfig),

    Name = create_random_name("app1"),
    Vsn = create_random_vsn(),
    create_lfe_app(AppDir, Name, Vsn, [kernel, stdlib]),

    [{app_names, [Name]}, {vsns, [Vsn]}|NewConfig].

end_per_group(_Group, _Config) ->
    ok.

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    case ?config(apps, Config) of
        undefined -> init_rebar_state(Config);
        _         -> Config
    end.

end_per_testcase(_, _Config) ->
    catch meck:unload().

%%% ============================================================= [ Test Cases ]

build_basic_lfe_app(Config) ->
    AppDir = ?config(apps, Config),
    [Name] = ?config(app_names, Config),
    {ok, RebarConfig} = file:consult(create_config(AppDir, ?LFE_APP_CONFIG)),
    run_and_check(Config, RebarConfig, ["compile"], {ok, [{app, Name}]}).

paths_basic_lfe_app(Config) ->
    [Name] = ?config(app_names, Config),
    [Vsn] = ?config(vsns, Config),

    {ok, State} = run_and_check(Config, [], ["compile"], return),

    code:add_paths(rebar_state:code_paths(State, all_deps)),
    ok = application:load(list_to_atom(Name)),
    Loaded = application:loaded_applications(),
    {_, _, Vsn} = lists:keyfind(list_to_atom(Name), 1, Loaded).

clean_basic_lfe_app(Config) ->
    [Name] = ?config(app_names, Config),

    run_and_check(Config, [], ["clean"], {ok, [{app, Name, invalid}]}).

%%% ==================================================================== [ EOF ]
