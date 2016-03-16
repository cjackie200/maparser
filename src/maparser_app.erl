-module (maparser_app).
-behaviour(application).
-include ("maparser.hrl").
-export([start/2,
        start_app/0,
        stop/1]).


start_app() ->
    application:start(maparser).

start(_, []) ->
        ok = init_ets(),
        init_config(),
        maparser_sup:start_link().

stop(_) ->
    io:format("maparser stop...~n"),
    ok.


init_ets() ->
    ets:new(?ETS_CONFIG, [set, public, {keypos, 1}, named_table, {read_concurrency, true}]),
    ets:new(?ETS_BUFF, [set, public, {keypos, 1}, named_table, {read_concurrency, true}]),
    ok.

init_config() ->
    InitConfig = application:get_all_env(),
    get_app_config(InitConfig).

get_app_config([]) ->
    ok;

get_app_config([H | T]) ->
    util:insert_config(H),
    get_app_config(T).
