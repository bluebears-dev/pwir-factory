-module(prod_line).
-compile([export_all]).

machine(_, NextMachinePid, _, _, CurrentAmount, _, _, _, Group, endline) ->
    receive
        {produce, _} ->
            NextMachinePid ! {completed, {Group, CurrentAmount + 1}}
    end,
    machine(null, NextMachinePid, null, null, CurrentAmount + 1, null, null, null, Group, endline);

machine(StoragePid, NextMachinePid, Component, Resource, CurrentAmount, Delay, RefillAmount, FillAmount, Group, Type) when CurrentAmount - FillAmount < 0 ->
    empty_machine(StoragePid, NextMachinePid, Component, Resource, CurrentAmount, Delay, RefillAmount, FillAmount, Group, Type);

machine(StoragePid, NextMachinePid, Component, Resource, CurrentAmount, Delay, RefillAmount, FillAmount, Group, beginning) when is_integer(Delay) ->
    timer:sleep(Delay),
    NextMachinePid ! {produce, [Resource]},
    user_interface ! {update, prod_line, {Group, Resource, CurrentAmount - FillAmount}},
    machine(StoragePid, NextMachinePid, Component, Resource, CurrentAmount - FillAmount, Delay, RefillAmount, FillAmount, Group, beginning);

machine(StoragePid, NextMachinePid, Component, Resource, CurrentAmount, Delay, RefillAmount, FillAmount, Group, worker) when is_integer(CurrentAmount) ->
    receive
        {produce, Product} when is_list(Product) ->
            case lists:member(Component, Product) of
                true ->
                    timer:sleep(Delay),
                    NextMachinePid ! {produce, Product++[Resource]},
                    user_interface ! {update, prod_line, {Group, Resource, CurrentAmount - FillAmount}},
                    machine(StoragePid, NextMachinePid, Component, Resource, CurrentAmount - FillAmount, Delay, RefillAmount, FillAmount, Group, worker);
                _ ->
                    machine(StoragePid, NextMachinePid, Component, Resource, CurrentAmount, Delay, RefillAmount, FillAmount, Group, worker)

            end
    end;

machine(_, NextMachinePid, _, _, CurrentAmount, _, _, _, Group, endline) ->
    receive
        {produce, _} ->
            NextMachinePid ! {completed, {Group, CurrentAmount + 1}}
    end,
    machine(null, NextMachinePid, null, null, CurrentAmount + 1, null, null, null, Group, endline).

empty_machine(StoragePid, NextMachinePid, Component, Resource, CurrentAmount, Delay, RefillAmount, FillAmount, Group, Type) -> 
    StoragePid ! {self(), request_storage, Resource, RefillAmount},
    receive
        {storage_delivery, Resource, Amount} when is_integer(Amount) ->
            user_interface ! {update, prod_line, {Group, Resource, CurrentAmount + RefillAmount}},
            machine(StoragePid, NextMachinePid, Component, Resource, CurrentAmount + Amount, Delay, RefillAmount, FillAmount, Group, Type);
        {storage_empty, Resource, WaitTime} when is_integer(WaitTime) ->
            timer:sleep(WaitTime),
            empty_machine(StoragePid, NextMachinePid, Component, Resource, CurrentAmount, Delay, RefillAmount, FillAmount, Group, Type)
    end.

create_machines(EndPointPid, [], Group, worker) ->
    spawn(prod_line, machine, [null, EndPointPid, null, null, 0, null, null, null, Group, endline]);

create_machines(EndPointPid, [MachineInfo|Machines], Group, worker) ->
    Pid = create_machines(EndPointPid, Machines, Group, worker),
    {StoragePid, Component, Resource, Delay, RefillAmount, FillAmount} = MachineInfo,
    spawn(prod_line, machine, [StoragePid, Pid, Component, Resource, 0, Delay, RefillAmount, FillAmount, Group, worker]).

create_machines(EndPointPid, [MachineInfo|Machines], Group) ->
    Pid = create_machines(EndPointPid, Machines, Group, worker),
    {StoragePid, Component, Resource, Delay, RefillAmount, FillAmount} = MachineInfo,
    spawn(prod_line, machine, [StoragePid, Pid, Component, Resource, 0, Delay, RefillAmount, FillAmount, Group, beginning]).
