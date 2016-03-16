-ifndef(MAP_LOGGER_HRL).
-define(MAP_LOGGER_HRL, ok).

-define(MAPARSER_EMYSQL, maparser_emysql).
-define(MYSQL_WRITE_NUMBER, 200000).

-define(ETS_CONFIG, ets_config).
-define(ETS_BUFF, ets_buff).

-record(buff, {
    terrain = "0",
    scene = "0",
    object = "0"
    }).

-endif.
