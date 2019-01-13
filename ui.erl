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

draw_resource(Resource, Amount, Limit, PosX, PosY) ->
    Name = string:pad(io_lib:format("~ts: ~B", [Resource, Amount]), 30, trailing),
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
    string:length(Name).


draw_resources([], _, _, _) ->
    ok;

draw_resources([{Resource, Amount}|Resources], Limit, PosX, PosY) ->
    Skip = draw_resource(Resource, Amount, Limit, PosX, PosY),
    draw_resources(Resources, Limit, PosX, PosY + Skip + 1).


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
    Name = io_lib:format("~ts", [RawName]),
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

get_index([], _, _) ->
    -1;

get_index([H|_], Element, Index) when H =:= Element ->
    Index;

get_index([_|T], Element, Index) ->
    get_index(T, Element, Index + 1).

draw_prod_lines([], _, _, _, _, _) ->
    ok;

draw_prod_lines([Member|ProdLines], Group, Resource, Amount, PosX, PosY) ->
    {Resources, LineGroup} = Member,
    SkipSize = get_index(Resources, Resource, 0) * 30,
    case LineGroup =:= Group of
        true ->
            draw_resource(Resource, Amount, 1, PosX, PosY + SkipSize + 1);
        _ ->
            draw_prod_lines(ProdLines, Group, Resource, Amount, PosX + 3, PosY)
    end.

draw_products([], _, _, _, _) ->
    ok;

draw_products([Member|ProdLines], Group, Amount, PosX, PosY) ->
    {Resources, LineGroup} = Member,
    case LineGroup =:= Group of
        true ->
            cecho:mvaddstr(PosX + 1, PosY + 1, io_lib:format("~ts: ~B", ["ilosc produktu", Amount])); 
        _ ->
            draw_products(ProdLines, Group, Amount, PosX + 3, PosY)
    end.


ui(Distributors, Storages, ProductionLines) ->
    receive
        {update, distributor, {Pid, Resource}} ->
            draw_distributors(Distributors, Pid, Resource, 3, 4);
        {update, storage, {Pid, Resources}, Limit} ->
            draw_storages(Storages, Pid, Resources, Limit, 6, 4); 
        {update, prod_line, {Group, Resource, Amount}} ->
            draw_prod_lines(ProductionLines, Group, Resource, Amount, 20, 4);
        {completed, {Group, Amount}} ->
            draw_products(ProductionLines, Group, Amount, 20, 4)
    end,
    cecho:refresh(),
    ui(Distributors, Storages, ProductionLines).

draw_dist_label(Distributors, PosX, PosY) ->
    cecho:mvaddstr(PosX, PosY, "Dystrybutorzy:"),
    lists:foreach(fun({Value, _}) -> draw_distributors(Distributors, Value, none, PosX + 1, PosY + 2) end, Distributors).

draw_element_label([], _, _, _) ->
    ok;

draw_element_label([{_,Name}|ComplexLabels], PosX, PosY, DeltaX) ->
    cecho:mvaddstr(PosX, PosY, io_lib:format("~ts", [Name])),
    draw_element_label(ComplexLabels, PosX + DeltaX, PosY, DeltaX).

draw_complex_label(ComplexLabels, MainLabel, PosX, PosY, DeltaX) ->
    cecho:mvaddstr(PosX, PosY, MainLabel),
    draw_element_label(ComplexLabels, PosX + 1, PosY + 2, DeltaX).

ui_input() ->
    cecho:refresh(),
    Key = cecho:getch(),
    case Key of
        $q ->
            cecho:echo(),
            cecho:nocbreak(),
            application:stop(cecho),
            erlang:halt();
        _ ->
            ui_input()
    end.


create_ui() ->
    {Distributors, Storages, ProductionLines} = receive
       V -> V
    end,
    init(),
    draw_dist_label(Distributors, 2, 2),
    draw_complex_label(Storages, "Magazyny", 5, 2, 2),
    draw_complex_label(ProductionLines, "Linie produkcyjne", 18, 2, 3),
    {My, Mx} = cecho:getmaxyx(),
    cecho:mvaddstr(My - 1, 0, string:pad(io_lib:format("~ts", ["q - Zakoncz program"]), Mx, both)),
    cecho:refresh(),
    spawn(ui, ui_input, []),
    ui(Distributors, Storages, ProductionLines).

