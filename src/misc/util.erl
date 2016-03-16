-module (util).
-include ("maparser.hrl").
-export ([
        rand/0,
        rand/2,
        now/0,
        now_seconds_without_pro/0,
        now_milliseconds_without_pro/0,
        get_config/1,
        insert_config/1,
        term_to_string/1,
        get_ets_value/2,
        formated_timestamp/0,
        sublist/2
        ]).



rand() ->
    <<Rand:32>> = crypto:strong_rand_bytes(4),
    Rand.
rand(Min, Max) ->
    Min + (rand() rem (Max - Min + 1)).

now() ->
    erlang:timestamp().

now_seconds_without_pro() ->
    {MegaSecs, Secs, _MicroSecs} = erlang:timestamp(),
    MegaSecs * 1000000 + Secs.

now_milliseconds_without_pro() ->
    {MegaSecs, Secs, MicroSecs} = erlang:timestamp(),
    NowMicroSecs = MegaSecs * 1000000000000 + Secs * 1000000 + MicroSecs,
    NowMicroSecs div 1000.

term_to_string(Term) ->
    binary_to_list(list_to_binary(io_lib:format("~p", [Term]))).

get_ets_value(Table, Key) ->
    ets:lookup(Table, Key).

get_config(Key) ->
    case get_ets_value(?ETS_CONFIG, Key) of
    [{_, Config}] ->
        Config;
    _ ->
        case application:get_env(Key) of
        {ok, Value} ->
            Value;
        _ ->
            undefined
        end
    end.

insert_config(Data) ->
    ets:insert(?ETS_CONFIG, Data).

% piont_of_rectangle({{X, Y}, {LenX, LenY}}) ->
%     piont_of_rectangle({X, Y}, {LenX, LenY}).

% piont_of_rectangle({X, Y}, {LenX, LenY}) ->
%     piont_of_rectangle(X, Y, LenY - 1, [], LenX - 1, LenY -1).

% piont_of_rectangle(X, Y, _, Res, 0, 0) ->
%     [{X, Y} | Res];

% piont_of_rectangle(X, Y, Sy, Res, Cx, 0) ->
%     NewRes = [{X + Cx, Y} | Res],
%     piont_of_rectangle(X, Y, Sy, NewRes, Cx -1, Sy);

% piont_of_rectangle(X, Y, Sy, Res, Cx, Cy) ->
%     NewRes = [{X + Cx, Y + Cy} | Res],
%     piont_of_rectangle(X, Y, Sy, NewRes, Cx, Cy - 1).

formated_timestamp() ->
    {Date, Time}   = erlang:localtime(),
    {YYYY,MM,DD}   = Date,
    {Hour,Min,Sec} = Time,
    FormatDate =
        io_lib:format("~.4w-~.2.0w-~.2.0w_~.2.0w-~.2.0w-~.2.0w",
                      [YYYY,MM,DD,Hour,Min,Sec]),
    lists:flatten(FormatDate).

sublist(List, N) ->
    sublist(List, N, []).

sublist(List, 0, Res) ->
    {Res, List};

sublist([H | T], N, Res) ->
    sublist(T, N -1, [H | Res]).


