-module (db_mysql).
-behaviour(gen_server).
-include ("maparser.hrl").

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-export([
    start_link/0
]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    process_flag(trap_exit, true),
    crypto:start(),
    ok = application:start(emysql),
    Re = add_pool(),
    io:format("emysql -- pool : ~p start ~p!~n", [?MAPARSER_EMYSQL, Re]),
    {ok, []}.

add_pool()->
    MysqlHost = util:get_config(mysql_host),
    MysqlPort = util:get_config(mysql_port),
    MysqlUserName = util:get_config(mysql_username),
    MysqlPassword = util:get_config(mysql_password),
    MysqlDatabase = util:get_config(mysql_database),
    emysql:add_pool(?MAPARSER_EMYSQL, [
        {size, 10},
        {host, MysqlHost},
        {port, MysqlPort},
        {user, MysqlUserName},
        {password, MysqlPassword},
        {database, MysqlDatabase},
        {encoding, utf8}]).


code_change(_OldVsn, Status, _Extra) ->
    {ok, Status}.

terminate(_Reason, Status) ->
    {ok, Status}.

handle_call(_Info, _From, Status) ->
    {reply, ok, Status}.


handle_cast(_Info, Status) ->
    {noreply, Status}.

handle_info(start, Status) ->
    io:format("write data start ... ~p~n", [util:formated_timestamp()]),
    write_data(),
    io:format("write data ok ... ~p~n", [util:formated_timestamp()]),
    Size = ets:info(?ETS_BUFF, size),
    io:format("write data end. total:~p~n", [Size]),
    {noreply, Status};

handle_info(_Info, Status) ->
    {noreply, Status}.


execute_sql(Query) ->
    emysql:execute(?MAPARSER_EMYSQL, Query).


write_data() ->
    case ets:info(?ETS_BUFF, size) of
    0 ->
        io:format("table size: 0 !~n");
    undefined ->
        io:format("table undefined!~n");
    _ ->
        Map = ets:tab2list(?ETS_BUFF),
        io:format("~p table len ~p~n", [?ETS_BUFF, length(Map)]),
        update_mysql(Map)
    end.


update_mysql(Map) when length(Map) =< ?MYSQL_WRITE_NUMBER ->
    Query = make_replace_mysql(Map),
    execute_sql(Query);

update_mysql(Map) ->
    {H, T} = util:sublist(Map, ?MYSQL_WRITE_NUMBER),
    Query = make_replace_mysql(H),
    execute_sql(Query),
    update_mysql(T).

make_replace_mysql(Value) ->
    try
    FieldValue = make_field_string(Value),
    "REPLACE INTO map(row,col,terrain,scene,object)"++" VALUES"++FieldValue
    catch
        _:R ->
            io:format("~p~n", [R]),
        ""
    end.


make_field_string(Value) ->
    make_field_string(Value, []).

make_field_string([], [_ | Res]) ->
    Res;

make_field_string([{Pos, #buff{
    terrain = Terrain,
    scene = Scene,
    object = Object
}} | T], Res) ->
    Point = pos_to_RowCol(Pos),
    make_field_string(T, ",("++Point++","++Terrain++","++Scene++","++Object++")"++Res).

pos_to_RowCol(Pos) ->
    Hight = util:get_config(height),
    lists:concat([Pos rem Hight, ",", Pos div Hight]).
