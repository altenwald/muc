-module(muc_test).
-compile([export_all]).

disco_test_() ->
    ecomponent_func_test:check([
        "disco_info"
    ]).
