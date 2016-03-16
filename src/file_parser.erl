-module (file_parser).
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
    start_link/0,
    stop/0
]).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

init(_) ->
    process_flag(trap_exit, true),
    observer:start(),
    erlang:send_after(0, ?MODULE, start),
    {ok, []}.

code_change(_OldVsn, Status, _Extra) ->
    {ok, Status}.

terminate(_Reason, Status) ->
    {ok, Status}.

handle_call(stop, _ , Status) ->
    {stop, normal, stopped, Status};

handle_call(_Info, _From, Status) ->
    {reply, ok, Status}.


handle_cast(_Info, Status) ->
    {noreply, Status}.

handle_info(start, _) ->
    io:format("parser start~n"),
    parser_file(),
    {noreply, []};

handle_info(shutdown, Status) ->
    {stop, normal, Status};

handle_info(_Info, Status) ->
    {noreply, Status}.

parser_file() ->
    File = util:get_config(map_file),
    io:format("FileName : ~p~n", [File]),
    case catch file:open(File, read) of
    {ok, Fid} ->
        parser_file(Fid);
    Error ->
        io:format("file open fail :~p~n", [Error])
    end.


parser_file(Fid) ->
    Data =  file:read_line(Fid),
    parser_file(Data, Fid).

parser_file(eof, Fid) ->
    {ok, FileName} = file:pid2name(Fid),
    io:format("file: ~p  parser ok!~n", [FileName]),
    db_mysql ! start;

parser_file({ok, Data}, Fid) ->
    parser_data(Data, Fid),
    parser_file(Fid);

parser_file(Error, _Fid) ->
    io:format("file parser fail :~p~n", [Error]),
    ok.

parser_data("[header]\n", Fid) ->
    parser_header(Fid);

parser_data("[layer]\n", Fid) ->
    parser_layer(Fid);

parser_data("[object]\n", Fid) ->
    parser_object(Fid);

parser_data(_, _) ->
    ok.

parser_header(Fid) ->
    case catch file:read_line(Fid) of
    {ok,"\n"} ->
        ok;
    {ok, Data} ->
        parser_header_data(Data),
        parser_header(Fid);
    _ ->
        ok
    end.

parser_header_data(Data) ->
    Concent = string:tokens(Data, "="),
    parser_header_concent(Concent).


parser_header_concent(["height", Str]) ->
    Number = str2number(Str),
    io:format("map height = ~p~n", [Number]),
    util:insert_config({height, Number});

parser_header_concent(["width", Str]) ->
    Number = str2number(Str),
    io:format("map width = ~p~n", [Number]),
    util:insert_config({width, Number});

parser_header_concent(["tilewidth", Str]) ->
        Number = str2number(Str),
        io:format("map tilewidth = ~p~n", [Number]),
        util:insert_config({tilewidth, Number});

parser_header_concent(["tileheight", Str]) ->
        Number = str2number(Str),
        io:format("map tileheight = ~p~n", [Number]),
        util:insert_config({tileheight, Number});

parser_header_concent(_) ->
    ok.

str2number(Str) ->
    list_to_integer(Str -- "\n").

parser_layer(Fid) ->
    {ok, Type} =  file:read_line(Fid),
    parser_layer_type(Type, Fid).

parser_layer_type("type=terrain\n", Fid) ->
    io:format("parser_layer_terrain~n"),
    {ok, Data} = file:read_line(Fid),
    parser_layer_terrain(Fid, 0, Data);

parser_layer_type("type=scene\n", Fid) ->
    io:format("parser_layer_scene~n"),
    {ok, Data} = file:read_line(Fid),
    parser_layer_scene(Fid, 0, Data);

parser_layer_type(_, _) ->
    ok.

parser_layer_terrain(_, Pos, "\n") ->
    io:format("parser_layer_terrain pos ~p~n", [Pos]),
    ok;

parser_layer_terrain(Fid, Pos, "data=\n") ->
    {ok, NewData} = file:read_line(Fid),
    parser_layer_terrain(Fid, Pos, NewData);

parser_layer_terrain(Fid, Pos, Data) ->
    Concent = string:tokens(Data, ","),
    Num = parser_layer_terrain_line(Pos, Concent),
    {ok, NewData} = file:read_line(Fid),
    parser_layer_terrain(Fid, Num, NewData).

parser_layer_terrain_line(Pos, ["\n"]) ->
    Pos;

parser_layer_terrain_line(Pos, [T | []]) ->
    update_terrain(Pos, T -- "\n"),
    Pos+1;

parser_layer_terrain_line(Pos, [H | T]) ->
    update_terrain(Pos, H),
    parser_layer_terrain_line(Pos+1, T).


update_terrain(Pos, Terrain) ->
    Buff =  get_table_value(Pos),
    NewBuff = Buff#buff{
        terrain = Terrain
    },
    insert_table_value(Pos, NewBuff).



parser_layer_scene(_, Pos, "\n") ->
    io:format("parser_layer_scene pos ~p~n", [Pos]),
    ok;

parser_layer_scene(Fid, Pos, "data=\n") ->
    {ok, NewData} = file:read_line(Fid),
    parser_layer_scene(Fid, Pos, NewData);

parser_layer_scene(Fid, Pos, Data) ->
    Concent = string:tokens(Data, ","),
    Num = parser_layer_scene_line(Pos, Concent),
    {ok, NewData} = file:read_line(Fid),
    parser_layer_scene(Fid, Num, NewData).

parser_layer_scene_line(Pos, ["\n"]) ->
    Pos;

parser_layer_scene_line(Pos, [T | []]) ->
    update_scene(Pos, T -- "\n"),
    Pos+1;

parser_layer_scene_line(Pos, [H | T]) ->
    update_scene(Pos, H),
    parser_layer_scene_line(Pos+1, T).


update_scene(Pos, Scene) ->
    Buff =  get_table_value(Pos),
    NewBuff = Buff#buff{
        scene = Scene
    },
    insert_table_value(Pos, NewBuff).


parser_object(Fid) ->
    file:read_line(Fid),
    Type = read_type(Fid),
    {Row, Col, Height, Width} = read_location(Fid),
    range_object(Type, Row, Col, Height, Width, Width).


read_type(Fid) ->
    {ok, [$t, $y, $p, $e, $= | TypeStr]} = file:read_line(Fid),
    str2number(TypeStr).


read_location(Fid) ->
   {ok, [$l, $o, $c, $a, $t, $i, $o, $n, $= | LocationStr]} = file:read_line(Fid),
    [Col, Row, Width, Height] = string:tokens(LocationStr, ","),
    {list_to_integer(Row), list_to_integer(Col), str2number(Height), list_to_integer(Width)}.


range_object(_Type, _Row, _Col, -1, _Width, _Pos) ->
    ok;

range_object(Type, Row, Col, Height, Width, -1) ->
    range_object(Type, Row, Col, Height - 1, Width, Width);

range_object(Type, Row, Col, Height, Width, Pos) ->
    update_object(Type, Row+Height, Col+Pos),
    range_object(Type, Row, Col, Height, Width, Pos - 1).


update_object(Type, Row, Col) ->
    Width = util:get_config(width),
    Key = Row*Width+Col,
    Buff = get_table_value(Key),
    NewBuff = Buff#buff{
        object = Type
    },
    insert_table_value(Key, NewBuff).

get_table_value(Key) ->
    case ets:lookup(?ETS_BUFF, Key) of
    [{_, Buff}] ->
        Buff;
    _ ->
        #buff{}
    end.

insert_table_value(Pos, Buff) ->
    Mag = util:get_config(map_mag),
    Width = util:get_config(width),
    R = (Pos div Width)*2,
    C = (Pos rem Width)*2,
    range_mag(Buff, R, C, Width*Mag, Mag-1, Mag-1, Mag-1).

range_mag(_Buff,  _R, _C, _Wmag, -1, _Width, _Pos) ->
    ok;

range_mag(Buff,  R, C, Wmag, Height, Width, -1) ->
    range_mag(Buff,  R, C, Wmag, Height -1, Width, Width);


range_mag(Buff,  R, C, Wmag, Height, Width, Pos) ->
    Key = (R + Height)*Wmag+C+Pos,
    ets:insert(?ETS_BUFF, {Key, Buff}),
    range_mag(Buff,  R, C, Wmag, Height, Width, Pos - 1).


