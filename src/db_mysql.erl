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

handle_info({start, Table}, Status) ->
    io:format("write data start ... ~p~n", [util:formated_timestamp()]),
    write_data(Table),
    io:format("write data ok ... ~p~n", [util:formated_timestamp()]),
    Size = ets:info(Table, size),
    io:format("write data end. total:~p~n", [Size]),
    {noreply, Status};

handle_info(_Info, Status) ->
    {noreply, Status}.


execute_sql(Query) ->
    emysql:execute(?MAPARSER_EMYSQL, Query).


write_data(Table) ->
    case ets:info(Table, size) of
    0 ->
        io:format("table size: 0 !~n");
    undefined ->
        io:format("table undefined!~n");
    _ ->
        Map = ets:tab2list(Table),
        io:format("~p table len ~p~n", [Table, length(Map)]),
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
    FieldValue = make_field_string(Value),
    "REPLACE INTO map(row,col,terrain,scene,object)"++" VALUES"++FieldValue.


make_field_string(Value) ->
    Width = util:get_config(width) * util:get_config(map_mag),
    make_field_string(Value, [], Width).

make_field_string([], [_ | Res], _Width) ->
    Res;

make_field_string([{Pos, #buff{
    terrain = Terrain,
    scene = Scene,
    object = Object
}} | T], Res, Width) ->
    Point = pos_to_RowCol(Pos, Width),
    NewRes = ",("++Point++","++Terrain++","++Scene++","++Object++")"++Res,
    make_field_string(T, NewRes, Width).

pos_to_RowCol(Pos, Width) ->
    lists:concat([Pos div Width, ",", Pos rem Width]).
