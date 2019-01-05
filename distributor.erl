-module(distributor).
-compile([export_all]).

distributor(Delay, Resources) when is_list(Resources), is_integer(Delay) -> 
    receive
        {Pid, request_distributor, Resource, Amount} when is_integer(Amount), Amount > 0 ->
            case lists:member(Resource, Resources) of 
                true ->
                    timer:sleep(Delay),
                    Pid ! {distributor_delivery, Resource, Amount};
                false ->
                    invalid
            end
    end,
    distributor(Delay, Resources).

main() ->
    Pid = spawn(distributor, distributor, [2000, [a,b,c]]),
    Pid ! {self(), g, 20},
    receive
        {distributor_delivery, g, Amount} -> kurla 
    end.
