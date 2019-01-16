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

main_loop() ->
    main_loop().

main([]) ->
    io:format("~ts~n~ts~n~ts~n~ts~n", ["Wybierz jeden z wariantow fabryki:", "1 - Wariant 1", "2 - Wariant 2", "3 - Wariant 3"]);

main([Factory|_]) ->
    UiPid = spawn(ui, create_ui, []),
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
    WaterStoragePid = spawn(storage, create_storage, [WaterDistPid, Waters, [100000, 100000, 100000], 15000, ?WATER_STORAGE_DELAY]),
    CapStoragePid = spawn(storage, create_storage, [CapDistPid, Caps, [100], 20, ?CAP_DIST_DELAY]),
    LabelStoragePid = spawn(storage, create_storage, [LabelDistPid, Labels, [100, 100, 100], 30, ?LABEL_STORAGE_DELAY]),
    
    case Factory of
        "1" ->
            ProdLineOne = [
                            {BottleStoragePid, ignore, pol_litrowa, 400, 100, 1},
                            {WaterStoragePid, pol_litrowa, niegazowana, 100, 5000, 500},
                            {CapStoragePid, niegazowana, nakretka, 150, 50, 1},
                            {LabelStoragePid, nakretka, etykieta_niegazowanej, 300, 20, 1}
                          ],

            ProdLineTwo = [
                            {BottleStoragePid, ignore, poltora_litrowa, 550, 50, 1},
                            {WaterStoragePid, poltora_litrowa, gazowana, 300, 5000, 1500},
                            {CapStoragePid, gazowana, nakretka, 150, 30, 1},
                            {LabelStoragePid, nakretka, etykieta_gazowanej, 400, 20, 1}
                          ],
            
            LabelOne = "Linia wody niegazowanej (wariant 1)",
            LabelTwo = "Linia wody gazowanej duzej (wariant 1)", 
            create_machines(user_interface, ProdLineOne, LabelOne),
            create_machines(user_interface, ProdLineTwo, LabelTwo),

            ProductionLines = [
                            {[pol_litrowa, niegazowana, nakretka, etykieta_niegazowanej], LabelOne},
                            {[poltora_litrowa, gazowana, nakretka, etykieta_gazowanej], LabelTwo}
                          ];
        "2" ->
            ProdLineOne = [
                            {BottleStoragePid, ignore, pol_litrowa, 200, 20, 1},
                            {WaterStoragePid, pol_litrowa, niegazowana, 900, 5000, 500},
                            {CapStoragePid, niegazowana, nakretka, 100, 20, 1},
                            {LabelStoragePid, nakretka, etykieta_niegazowanej, 100, 10, 1}
                          ],

            ProdLineTwo = [
                            {BottleStoragePid, ignore, poltora_litrowa, 200, 40, 1},
                            {WaterStoragePid, poltora_litrowa, gazowana, 300, 15000, 1500},
                            {CapStoragePid, gazowana, nakretka, 100, 25, 1},
                            {LabelStoragePid, nakretka, etykieta_gazowanej, 200, 15, 1}
                          ],

            LabelOne = "Linia wody niegazowanej (wariant 2)",
            LabelTwo = "Linia wody gazowanej duzej (wariant 2)", 
            create_machines(user_interface, ProdLineOne, LabelOne),
            create_machines(user_interface, ProdLineTwo, LabelTwo),

            ProductionLines = [
                            {[pol_litrowa, niegazowana, nakretka, etykieta_niegazowanej], LabelOne},
                            {[poltora_litrowa, gazowana, nakretka, etykieta_gazowanej], LabelTwo}
                          ];
        "3" -> 
            ProdLineOne = [
                            {BottleStoragePid, ignore, pol_litrowa, 100, 50, 1},
                            {WaterStoragePid, pol_litrowa, niegazowana, 100, 5000, 500},
                            {CapStoragePid, niegazowana, nakretka, 100, 30, 1},
                            {LabelStoragePid, nakretka, etykieta_niegazowanej, 100, 20, 1}
                          ],

            ProdLineTwo = [
                            {BottleStoragePid, ignore, poltora_litrowa, 100, 30, 1},
                            {WaterStoragePid, poltora_litrowa, gazowana, 300, 5000, 1500},
                            {CapStoragePid, gazowana, nakretka, 100, 40, 1},
                            {LabelStoragePid, nakretka, etykieta_gazowanej, 100, 30, 1}
                          ],

            LabelOne = "Linia wody niegazowanej (wariant 3)",
            LabelTwo = "Linia wody gazowanej duzej (wariant 3)", 
            create_machines(user_interface, ProdLineOne, LabelOne),
            create_machines(user_interface, ProdLineTwo, LabelTwo),

            ProductionLines = [
                            {[pol_litrowa, niegazowana, nakretka, etykieta_niegazowanej], LabelOne},
                            {[poltora_litrowa, gazowana, nakretka, etykieta_gazowanej], LabelTwo}
                          ];
        _ ->
            ProductionLines = [],
            io:format("~ts~n~ts~n~ts~n~ts~n", ["Wybierz jeden z wariantow fabryki:", "1 - Wariant 1", "2 - Wariant 2", "3 - Wariant 3"]),
            erlang:halt()
    end,
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
    user_interface ! {Distributors, Storages, ProductionLines},
    main_loop().

