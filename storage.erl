-module(storage).
-compile([export_all]).

check_resources_amount(_, _, []) ->
    ok;
check_resources_amount(DistributorPid, Limit, [Value | Resources]) when is_integer(Limit) ->
    {Resource, Amount} = Value,
    case Amount =< Limit of
        true ->
            DistributorPid ! {self(), request_distributor, Resource, 1000};
        false ->
            false
    end,
    check_resources_amount(DistributorPid, Limit, Resources).


storage(DistributorPid, Resources, Limit, Delay) when is_list(Resources), is_integer(Delay), is_integer(Limit) ->
    check_resources_amount(DistributorPid, Limit, Resources),
    receive
        {Pid, request_storage, Resource, DeliveryAmount} when is_integer(DeliveryAmount), DeliveryAmount > 0 ->
            Value = lists:search(fun ({T, _}) -> T == Resource end, Resources),
            case Value of
                false ->
                    invalid,
                    storage(DistributorPid, Resources, Limit, Delay);
                {_, {_, Amount}} when Amount >= DeliveryAmount ->
                    timer:sleep(Delay),
                    Pid ! {storage_delivery, Resource, DeliveryAmount},
                    NewResources = lists:keystore(Resource, 1, Resources, {Resource, Amount - DeliveryAmount}),
                    storage(DistributorPid, NewResources, Limit, Delay);
                {_, {_, Amount}} when Amount < DeliveryAmount ->
                    Pid ! {storage_empty, Resource, 2000},
                    storage(DistributorPid, Resources, Limit, Delay)
            end;
        {distributor_delivery, Resource, DeliveryAmount} when is_integer(DeliveryAmount), DeliveryAmount > 0 ->
            Value = lists:search(fun ({T, _}) -> T == Resource end, Resources),
            case Value of
                false ->
                    invalid,
                    storage(DistributorPid, Resources, Limit, Delay);
                {_, {_, Amount}} -> 
                    NewResources = lists:keystore(Resource, 1, Resources, {Resource, Amount + DeliveryAmount}),
                    storage(DistributorPid, NewResources, Limit, Delay)
            end
    end.

create_storage(ResourceTypes, ResourceAmounts, Limit, Delay) ->
    receive
        {Pid, register_distributor} ->
            storage(Pid, lists:zip(ResourceTypes, ResourceAmounts), Limit, Delay) 
    end.
