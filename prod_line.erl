-module(prod_line).
-compile([export_all]).

machine(StoragePid, NextMachinePid, Component, Resource, 0, Delay, RefillAmount) ->
    empty_machine(StoragePid, NextMachinePid, Component, Resource, Delay, RefillAmount);

machine(StoragePid, NextMachinePid, Component, Resource, CurrentAmount, Delay, RefillAmount) when is_integer(CurrentAmount), CurrentAmount > 0 ->
    receive
        {produce, Product} when is_list(Product) ->
            case lists:member(Component, Product) of
                true ->
                    timer:sleep(Delay),
                    NextMachinePid ! {produce, Product++[Resource]},
                    machine(StoragePid, NextMachinePid, Component, Resource, CurrentAmount - 1, Delay, RefillAmount);
                _ ->
                    machine(StoragePid, NextMachinePid, Component, Resource, CurrentAmount, Delay, RefillAmount)

            end
    end.

empty_machine(StoragePid, NextMachinePid, Component, Resource, Delay, RefillAmount) -> 
    StoragePid ! {self(), request_storage, Resource, RefillAmount},
    receive
        {storage_delivery, Resource, Amount} when is_integer(Amount) ->
            machine(StoragePid, NextMachinePid, Component, Resource, Amount, Delay, RefillAmount);
        {storage_empty, Resource, WaitTime} when is_integer(WaitTime) ->
            timer:sleep(WaitTime),
            empty_machine(StoragePid, NextMachinePid, Component, Resource, Delay, RefillAmount)
    end.
