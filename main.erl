-module(main).
-compile([export_all]).

-import(distributor, [distributor/2]).
-import(storage, [create_storage/5]).
-import(prod_line, [machine/7]).

send_bottle(Pid) -> 
    Pid ! {produce, [bottle]},
    timer:sleep(100),
    send_bottle(Pid).

collect() ->
    receive
        {produce, Product} ->
            io:format("~p finalized~n", [Product])
    end,
    collect().

main() ->
    LabelDistPid = spawn(distributor, distributor, [3000, [label]]),
    LabelStoragePid = spawn(storage, create_storage, [LabelDistPid, [label], [50], 20, 500]),
    LabelMachinePid = spawn(prod_line, machine, [LabelStoragePid, spawn(main, collect, []), gaz, label, 0, 500, 10]), 

    WaterDistPid = spawn(distributor, distributor, [2000, [gaz, ngaz]]),
    WaterStoragePid = spawn(storage, create_storage, [WaterDistPid, [gaz, ngaz], [1000, 1000], 100, 1000]),
    WaterMachinePid = spawn(prod_line, machine, [WaterStoragePid, LabelMachinePid, bottle, gaz, 0, 100, 100]),

    send_bottle(WaterMachinePid).
