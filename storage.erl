-module(storage).
-compile([export_all]).

-define(MACHINE_WAIT_TIME, 2000).
-define(REFILL_MULT, 5).

check_resources_amount(_, _, [], Requests) ->
    Requests;
check_resources_amount(DistributorPid, Limit, [Value | Resources], Requests) when is_integer(Limit) ->
    {Resource, Amount} = Value,
    case {Amount =< Limit, lists:member(Resource, Requests)} of
        {true, false} ->
            DistributorPid ! {self(), request_distributor, Resource, ?REFILL_MULT * Limit},
            check_resources_amount(DistributorPid, Limit, Resources, Requests++[Resource]);
        _ ->
            check_resources_amount(DistributorPid, Limit, Resources, Requests)
    end.

storage(DistributorPid, Resources, Limit, Delay, Requests) when is_list(Resources), is_integer(Delay), is_integer(Limit) ->
    NewRequests = check_resources_amount(DistributorPid, Limit, Resources, Requests),
    receive
        {Pid, request_storage, Resource, DeliveryAmount} when is_integer(DeliveryAmount), DeliveryAmount > 0 ->
            Value = lists:search(fun ({T, _}) -> T == Resource end, Resources),
            case Value of
                {_, {_, Amount}} when Amount >= DeliveryAmount ->
                    timer:sleep(Delay),
                    Pid ! {storage_delivery, Resource, DeliveryAmount},
                    NewResources = lists:keystore(Resource, 1, Resources, {Resource, Amount - DeliveryAmount}),
                    user_interface ! {update, storage, {self(), NewResources}, Limit},
                    storage(DistributorPid, NewResources, Limit, Delay, NewRequests);
                {_, {_, Amount}} when Amount < DeliveryAmount ->
                    Pid ! {storage_empty, Resource, ?MACHINE_WAIT_TIME},
                    storage(DistributorPid, Resources, Limit, Delay, NewRequests);
                _ ->
                    storage(DistributorPid, Resources, Limit, Delay, NewRequests)
            end;
        {distributor_delivery, Resource, DeliveryAmount} when is_integer(DeliveryAmount), DeliveryAmount > 0 ->
            Value = lists:search(fun ({T, _}) -> T == Resource end, Resources),
            case Value of
                {_, {_, Amount}} -> 
                    NewResources = lists:keystore(Resource, 1, Resources, {Resource, Amount + DeliveryAmount}),
                    user_interface ! {update, storage, {self(), NewResources}, Limit},
                    storage(DistributorPid, NewResources, Limit, Delay, lists:delete(Resource, NewRequests));
                _ ->
                    storage(DistributorPid, Resources, Limit, Delay, NewRequests)
            end
    end.

create_storage(Pid, ResourceTypes, ResourceAmounts, Limit, Delay) ->
    storage(Pid, lists:zip(ResourceTypes, ResourceAmounts), Limit, Delay, []). 
