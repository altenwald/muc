-module(muc).
-compile([warnings_as_errors]).

-export([process_iq/1]).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("ecomponent/include/ecomponent.hrl").

process_iq(#params{from=From, iq=IQ, ns=?NS_DISCO_ITEMS, type="get"}) ->
    FromBin = exmpp_jid:bare_to_binary(exmpp_jid:make(From)),
    lager:debug("get room list", []),
    Rooms = muc_db:list_rooms(FromBin),
    Items = lists:map(fun([Name, JID]) ->
        exmpp_xml:element(undefined, <<"item">>, [
        	{<<"jid">>, JID}, {<<"name">>, Name}
        ], [])
    end, Rooms),
    Result = exmpp_iq:result(IQ, Items),
    ecomponent:send(Result, ?NS_DISCO_ITEMS, muc, false),
    ok;

process_iq(#params{}=Params) ->
    lager:info("Params arrived: ~p~n", [Params]),
    ok.
