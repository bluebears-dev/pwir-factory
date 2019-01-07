-module(prod_line).
-compile([export_all]).

machine(StoragePid, NextMachinePid, Component, Resource, 0, Delay, RefillAmount, FillAmount, Group, Type) ->
    empty_machine(StoragePid, NextMachinePid, Component, Resource, Delay, RefillAmount, FillAmount, Group, Type);

machine(StoragePid, NextMachinePid, Component, Resource, CurrentAmount, Delay, RefillAmount, FillAmount, Group, beginning) when is_integer(Delay) ->
    timer:sleep(Delay),
    NextMachinePid ! {produce, [Resource]},
    machine(StoragePid, NextMachinePid, Component, Resource, CurrentAmount - FillAmount, Delay, RefillAmount, FillAmount, Group, beginning);

machine(StoragePid, NextMachinePid, Component, Resource, CurrentAmount, Delay, RefillAmount, FillAmount, Group, worker) when is_integer(CurrentAmount), CurrentAmount > 0 ->
    receive
        {produce, Product} when is_list(Product) ->
            case lists:member(Component, Product) of
                true ->
                    timer:sleep(Delay),
                    NextMachinePid ! {produce, Product++[Resource]},
                    machine(StoragePid, NextMachinePid, Component, Resource, CurrentAmount - FillAmount, Delay, RefillAmount, FillAmount, Group, worker);
                _ ->
                    machine(StoragePid, NextMachinePid, Component, Resource, CurrentAmount, Delay, RefillAmount, FillAmount, Group, worker)

            end
    end.

empty_machine(StoragePid, NextMachinePid, Component, Resource, Delay, RefillAmount, FillAmount, Group, Type) -> 
    StoragePid ! {self(), request_storage, Resource, RefillAmount},
    receive
        {storage_delivery, Resource, Amount} when is_integer(Amount) ->
            machine(StoragePid, NextMachinePid, Component, Resource, Amount, Delay, RefillAmount, FillAmount, Group, Type);
        {storage_empty, Resource, WaitTime} when is_integer(WaitTime) ->
            timer:sleep(WaitTime),
            empty_machine(StoragePid, NextMachinePid, Component, Resource, Delay, RefillAmount, FillAmount, Group, Type)
    end.

create_machines(EndPointPid, [], _, worker) ->
    EndPointPid;

create_machines(EndPointPid, [MachineInfo|[]], Group, worker) ->
    {StoragePid, Component, Resource, Delay, RefillAmount, FillAmount} = MachineInfo,
    spawn(prod_line, machine, [StoragePid, EndPointPid, Component, Resource, 0, Delay, RefillAmount, FillAmount, Group, worker]);

create_machines(EndPointPid, [MachineInfo|Machines], Group, worker) ->
    Pid = create_machines(EndPointPid, Machines, Group, worker),
    {StoragePid, Component, Resource, Delay, RefillAmount, FillAmount} = MachineInfo,
    spawn(prod_line, machine, [StoragePid, Pid, Component, Resource, 0, Delay, RefillAmount, FillAmount, Group, worker]).

create_machines(EndPointPid, [MachineInfo|Machines], Group) ->
    Pid = create_machines(EndPointPid, Machines, Group, worker),
    {StoragePid, Component, Resource, Delay, RefillAmount, FillAmount} = MachineInfo,
    spawn(prod_line, machine, [StoragePid, Pid, Component, Resource, 0, Delay, RefillAmount, FillAmount, Group, beginning]).
