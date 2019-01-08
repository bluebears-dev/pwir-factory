-module(factory).
-compile([export_all]).

-mode(compile).

-import(distributor, [distributor/2]).
-import(storage, [create_storage/5]).
-import(prod_line, [create_machines/3]).
-import(ui, [create_ui/3]).

-define(BOTTLE_DIST_DELAY, 5000).
-define(BOTTLE_STORAGE_DELAY, 1000).

-define(WATER_DIST_DELAY, 2000).
-define(WATER_STORAGE_DELAY, 1000).

-define(CAP_DIST_DELAY, 6000).

-define(LABEL_DIST_DELAY, 3000).
-define(LABEL_STORAGE_DELAY, 500).


main() ->
    process_flag(trap_exit, true),
    UiPid = spawn_link(ui, create_ui, []),
    register(user_interface, UiPid),

    Bottles = [pol_litrowa, poltora_litrowa],
    Waters = [niegazowana, gazowana, lekko_gazowana],
    Labels = [etykieta_niegazowanej, etykieta_gazowanej, etykieta_lekkogazowanej],
    Caps = [nakretka],

    BottleDistPid = spawn(distributor, distributor, [?BOTTLE_DIST_DELAY, Bottles]),
    WaterDistPid = spawn(distributor, distributor, [?WATER_DIST_DELAY, Waters]),
    CapDistPid = spawn(distributor, distributor, [?CAP_DIST_DELAY, Caps]),
    LabelDistPid = spawn(distributor, distributor, [?LABEL_DIST_DELAY, Labels]),

    BottleStoragePid = spawn(storage, create_storage, [BottleDistPid, Bottles, [100, 100], 20, ?BOTTLE_STORAGE_DELAY]),
    WaterStoragePid = spawn(storage, create_storage, [WaterDistPid, Waters, [100000, 100000, 100000], 5000, ?WATER_STORAGE_DELAY]),
    CapStoragePid = spawn(storage, create_storage, [CapDistPid, Caps, [100], 20, ?CAP_DIST_DELAY]),
    LabelStoragePid = spawn(storage, create_storage, [LabelDistPid, Labels, [100, 100, 100], 10, ?LABEL_STORAGE_DELAY]),

    ProdLineOne = [
                    {BottleStoragePid, ignore, pol_litrowa, 200, 20, 1},
                    {WaterStoragePid, pol_litrowa, niegazowana, 300, 5000, 500},
                    {CapStoragePid, niegazowana, nakretka, 100, 20, 1},
                    {LabelStoragePid, nakretka, etykieta_niegazowanej, 100, 10, 1}
                  ],

    ProdLineTwo = [
                    {BottleStoragePid, ignore, poltora_litrowa, 200, 20, 1},
                    {WaterStoragePid, poltora_litrowa, gazowana, 300, 5000, 500},
                    {CapStoragePid, gazowana, nakretka, 100, 20, 1},
                    {LabelStoragePid, nakretka, etykieta_gazowanej, 100, 10, 1}
                  ],

    create_machines(user_interface, ProdLineOne, "Linia wody niegazowanej"),
    create_machines(user_interface, ProdLineTwo, "Linia wody gazowanej duzej"),

    Distributors = [
                    {BottleDistPid, "Butelki"},
                    {WaterDistPid, "Woda"},
                    {CapDistPid, "Nakretki"},
                    {LabelDistPid, "Etykiety"}
                   ],
    Storages = [
                {BottleStoragePid, "Butelki"},
                {WaterStoragePid, "Woda"},
                {CapStoragePid, "Nakretki"},
                {LabelStoragePid, "Etykiety"}
               ],
    ProductionLines = [
                        {[pol_litrowa, niegazowana, nakretka, etykieta_niegazowanej], "Linia wody niegazowanej"},
                        {[poltora_litrowa, gazowana, nakretka, etykieta_gazowanej], "Linia wody gazowanej duzej"}
                      ],
    user_interface ! {Distributors, Storages, ProductionLines},
    receive
        {'EXIT', _, _} ->
            cecho:nocbreak(),
            application:stop(cecho),
            erlang:halt()
    end.

