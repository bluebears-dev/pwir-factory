-module(distributor).
-compile([export_all]).

distributor(Delay, Resources) when is_list(Resources), is_integer(Delay) -> 
    receive
        {Pid, request_distributor, Resource, Amount} when is_integer(Amount), Amount > 0 ->
            case lists:member(Resource, Resources) of 
                true ->
                    timer:sleep(Delay),
                    io:format("(~p) distributor sending ~s~n", [self(), Resource]),
                    Pid ! {distributor_delivery, Resource, Amount};
                false ->
                    io:format("(~p) invalid request~n", [self()]) 
            end
    end,
    distributor(Delay, Resources).

