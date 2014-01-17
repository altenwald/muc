-module(muc).
-compile([warnings_as_errors]).

-export([process_iq/1]).

-include_lib("ecomponent/include/ecomponent.hrl").

process_iq(#params{}=Params) ->
    lager:info("Params arrived: ~p~n", [Params]),
    ok.
