%% NB: copied verbatim from Tim Watson's hamcrest-erlang project, released under
%% the MIT license.
-define(CT_REGISTER_TESTS(Mod),
        All = [ FName || {FName, _} <- lists:filter(
            fun ({module_info,_}) -> false ;
                ({all,_}) -> false ;
                ({init_per_suite,1}) -> false ;
                ({end_per_suite,1}) -> false ;
                ({_,1}) -> true ;
                ({_,_}) -> false
            end,
            Mod:module_info(exports)
        )
    ],
    ct:pal("registering ~p~n", [All]),
    All).
