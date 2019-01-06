-module(ui).
-compile([export_all]).

-include_lib("cecho/include/cecho.hrl").

init() ->
    application:start(cecho),
    cecho:cbreak(),
    cecho:noecho(),
    cecho:curs_set(?ceCURS_INVISIBLE),
    HasColor = cecho:has_colors(),
    case HasColor of
        true -> 
            cecho:start_color(),
            cecho:init_pair(1, ?ceCOLOR_RED, ?ceCOLOR_BLACK),
            cecho:init_pair(2, ?ceCOLOR_GREEN, ?ceCOLOR_BLACK);
        false -> ignore
    end,
    HasColor.

draw_resources([], _, _, _) ->
    ok;

draw_resources([{Resource, Amount}|Resources], Limit, PosX, PosY) ->
    Name = string:pad(io_lib:format("~p: ~B", [Resource, Amount]), 30, trailing),
    case Amount >= Limit  of
        true ->
            cecho:attron(?ceCOLOR_PAIR(2)),
            cecho:mvaddstr(PosX, PosY, Name),
            cecho:attroff(?ceCOLOR_PAIR(2));
        false ->
            cecho:attron(?ceCOLOR_PAIR(1)),
            cecho:mvaddstr(PosX, PosY, Name),
            cecho:attroff(?ceCOLOR_PAIR(1))
    end,
    draw_resources(Resources, Limit, PosX, PosY + string:length(Name) + 1).


draw_storages([], _, _, _, _, _) ->
    ok;

draw_storages([Member|Storages], Pid, Resources, Limit, PosX, PosY) ->
    {DistPid, _} = Member,
    case DistPid =:= Pid of
        true ->
            draw_resources(Resources, Limit, PosX + 1, PosY + 2);
        _ ->
            draw_storages(Storages, Pid, Resources, Limit, PosX + 2, PosY)
    end.

draw_distributors([], _, _, _, _) ->
    ok;

draw_distributors([Member|Distributors], Pid, Resource, PosX, PosY) ->
    {DistPid, RawName} = Member,
    Name = io_lib:format("~p", [RawName]),
    case {DistPid =:= Pid, Resource} of
        {true, none} ->
            cecho:attron(?ceCOLOR_PAIR(2)),
            cecho:mvaddstr(PosX, PosY, Name),
            cecho:attroff(?ceCOLOR_PAIR(2));
        {true, _} ->
            cecho:attron(?ceCOLOR_PAIR(1)),
            cecho:mvaddstr(PosX, PosY, Name),
            cecho:attroff(?ceCOLOR_PAIR(1));
        _ ->
            draw_distributors(Distributors, Pid, Resource, PosX, PosY + string:length(Name) + 1)
    end.

ui(Distributors, Storages, ProductionLines) ->
    receive
        {update, distributor, {Pid, Resource}} ->
            draw_distributors(Distributors, Pid, Resource, 3, 4);
        {update, storage, {Pid, Resources}, Limit} ->
            draw_storages(Storages, Pid, Resources, Limit, 6, 4) 
    end,
    cecho:refresh(),
    ui(Distributors, Storages, ProductionLines).

init_distributors(Distributors, PosX, PosY) ->
    cecho:mvaddstr(PosX, PosY, "Distributors:"),
    lists:foreach(fun({Value, _}) -> draw_distributors(Distributors, Value, none, PosX + 1, PosY + 2) end, Distributors).

init_storages_util([], _, _) ->
    ok;

init_storages_util([{_,Name}|Storages], PosX, PosY) ->
    cecho:mvaddstr(PosX, PosY, io_lib:format("~p", [Name])),
    init_storages_util(Storages, PosX + 2, PosY).

init_storages(Storages, PosX, PosY) ->
    cecho:mvaddstr(PosX, PosY, "Storages:"),
    init_storages_util(Storages, PosX + 1, PosY + 2).

create_ui(Distributors, Storages, ProductionLines) ->
    init(),
    init_distributors(Distributors, 2, 2),
    init_storages(Storages, 5, 2),
    cecho:refresh(),
    ui(Distributors, Storages, ProductionLines).

