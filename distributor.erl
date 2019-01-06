-module(distributor).
-compile([export_all]).

distributor(Delay, Resources) when is_list(Resources), is_integer(Delay) -> 
    receive
        {Pid, request_distributor, Resource, Amount} when is_integer(Amount), Amount > 0 ->
            case lists:member(Resource, Resources) of 
                true ->
                    user_interface ! {update, distributor, {self(), Resource}},
                    timer:sleep(Delay),
                    Pid ! {distributor_delivery, Resource, Amount};
                _ ->
                    ignore
            end
    end,
    user_interface ! {update, distributor, {self(), none}},
    distributor(Delay, Resources).

