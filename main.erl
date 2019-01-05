-module(main).
-compile([export_all]).

-import(distributor, [distributor/2]).
-import(storage, [create_storage/4]).

main() ->
    WaterDistPid = spawn(distributor, distributor, [2000, [gaz, ngaz]]),
    WaterStorage = spawn(storage, create_storage, [[gaz, ngaz], [1000, 1000], 100, 1000]),
    WaterStorage ! {WaterDistPid, register_distributor},
    WaterStorage ! {self(), request_storage, gaz, 1000},
    receive
        {storage_delivery, gaz, Amount} ->
            Amount
    end.
