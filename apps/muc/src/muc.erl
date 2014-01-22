-module(muc).
-compile([warnings_as_errors]).

-export([
    process_iq/1,
    process_presence/1
]).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("ecomponent/include/ecomponent.hrl").
-include("muc.hrl").

%% Request disco#items for get all rooms:

process_iq(#params{from=From, to={undefined,_,_}, iq=IQ, ns=?NS_DISCO_ITEMS, type="get"}) ->
    FromBin = exmpp_jid:bare_to_binary(exmpp_jid:make(From)),
    lager:debug("get room list for ~s~n", [FromBin]),
    Rooms = muc_db:list_rooms(FromBin),
    Items = lists:map(fun({Name, JID}) ->
        exmpp_xml:element(undefined, 'item', [
        	exmpp_xml:attribute(<<"jid">>, JID),
            exmpp_xml:attribute(<<"name">>, Name)
        ], [])
    end, Rooms),
    Query = exmpp_xml:element(?NS_DISCO_ITEMS, 'query', [], Items),
    Result = exmpp_iq:result(IQ, Query),
    ecomponent:send(Result, ?NS_DISCO_ITEMS, muc, false),
    ok;

%% Request disco#info for get specific information about a room:

process_iq(#params{from=From, to={Room,_,undefined}, iq=IQ, ns=?NS_DISCO_INFO, type="get"}) ->
    FromBin = exmpp_jid:bare_to_binary(exmpp_jid:make(From)),
    lager:debug("get room [~s] info for ~s~n", [Room, FromBin]),
    case muc_db:get_room_info(FromBin, Room) of
    {ok, RoomInfo} ->
        Identity = exmpp_xml:element(undefined, 'identity', [
                exmpp_xml:attribute(<<"category">>, <<"conference">>),
                exmpp_xml:attribute(<<"name">>, RoomInfo#room_info.description),
                exmpp_xml:attribute(<<"type">>, <<"text">>)
            ], []),
        Features = get_features(RoomInfo),
        Fields = lists:map(fun({Var, Type, Label, Value}) ->
            set_field(Var, Type, Label, Value)
        end, [
            {<<"FORM_TYPE">>, <<"hidden">>, none, 
                <<"http://jabber.org/protocol/muc#roominfo">>},
            {<<"muc#roominfo_description">>, none, <<"Description">>, 
                RoomInfo#room_info.description},
            {<<"muc#roominfo_changesubject">>, none, 
                <<"Occupants May Change the Subject">>, 
                change_subject_occupants(RoomInfo#room_info.change_subject)},
            % TODO: needed?
            % {<<"muc#roominfo_contactjid">>, none,
            %    <<"Contact Addresses">>, ...},
            {<<"muc#roominfo_subject">>, none,
                <<"Current Discussion Topic">>, RoomInfo#room_info.subject},
            {<<"muc#roomconfig_changesubject">>, none,
                <<"Subject can be modified">>,
                change_subject(RoomInfo#room_info.change_subject)},
            {<<"muc#roominfo_occupants">>, none,
                <<"Number of occupants">>, muc_db:get_occupants_number(FromBin, Room)},
            {<<"muc#roominfo_lang">>, none,
                <<"Language of discussion">>, RoomInfo#room_info.language},
            {<<"muc#maxhistoryfetch">>, none,
                <<"Maximum Number of History Messages Returned by Room">>,
                RoomInfo#room_info.history_size}
        ]),
        Data = exmpp_xml:element('jabber:x:data', 'x', [
            exmpp_xml:attribute(<<"type">>, <<"result">>)], Fields),
        Result = exmpp_iq:result(IQ, [Data|Features] ++ [Identity]),
        ecomponent:send(Result, ?NS_DISCO_ITEMS, muc, false),
        ok;
    {error, notfound} ->
        lager:debug("IQ = ~p~n", [IQ]),
        Error = get_error('item-not-found'),
        Result = exmpp_iq:error(IQ, Error),
        ecomponent:send(Result, ?NS_DISCO_ITEMS, muc, false),
        ok
    end;

%% Request disco#items for get all users from a room:

process_iq(#params{from=From, to={Room,_,_}, iq=IQ, ns=?NS_DISCO_ITEMS, type="get"}) ->
    FromBin = exmpp_jid:bare_to_binary(exmpp_jid:make(From)),
    lager:debug("get room [~s] occupants info for ~s~n", [Room, FromBin]),
    Occupants = muc_db:get_occupants(FromBin, Room),
    Items = lists:map(fun({Name, JID}) ->
        set_item(Name, JID)
    end, Occupants),
    Query = exmpp_xml:element(?NS_DISCO_ITEMS, 'query', [], Items),
    Result = exmpp_iq:result(IQ, Query),
    ecomponent:send(Result, ?NS_DISCO_ITEMS, muc, false),
    ok;

%% Request disco#info for get the user information:

process_iq(#params{from=From, to={Room,_,Nick}, iq=IQ, ns=?NS_DISCO_INFO, type="get"}) ->
    FromBin = exmpp_jid:bare_to_binary(exmpp_jid:make(From)),
    lager:debug("get room [~s] user [~s] info for ~s~n", [Room, Nick, FromBin]),
    %% TODO: should return the detailed information about the user?
    Query = exmpp_xml:element(?NS_DISCO_ITEMS, 'query', [], []),
    Result = exmpp_iq:result(IQ, Query),
    ecomponent:send(Result, ?NS_DISCO_ITEMS, muc, false),
    ok;

process_iq(#params{iq=IQ}=Params) ->
    lager:error("bad-request for params: ~p~n", [Params]),
    Error = get_error('bad-request'),
    Result = exmpp_iq:error(IQ, Error),
    ecomponent:send(Result, ?NS_DISCO_ITEMS, muc, false),
    ok.

%% Create a room

process_presence(#presence{from=From, type="available", to={Room,_,Nick}, xmlel=_Xmlel}) ->
    FromBin = exmpp_jid:bare_to_binary(exmpp_jid:make(From)),
    lager:debug("get room [~s] user [~s] info for ~s~n", [Room, Nick, FromBin]),
    ok;

process_presence(Params) ->
    lager:error("bad-presence for params: ~p~n", [Params]),
    ok.

%% --------------------------------------------------------------------------
%% Internal functions:

change_subject_occupants(all) -> <<"true">>;
change_subject_occupants(_) -> <<"false">>.

change_subject(none) -> <<"false">>;
change_subject(_) -> <<"true">>.

get_features(_RoomInfo) ->
    % TODO: add features (when it's supported) as:
    % muc_passwordprotected
    % muc_hidden
    % muc_temporary
    % muc_open
    % muc_unmoderated
    % muc_nonanonymous
    lists:map(fun(Feature) ->
        exmpp_xml:element(undefined, 'feature', [
            exmpp_xml:attribute(<<"var">>, Feature)], [])
    end, [
        <<"http://jabber.org/protocol/muc">>
    ]).

set_item(Name, JID) ->
    exmpp_xml:element(undefined, 'item', [
        exmpp_xml:attribute(<<"name">>, Name),
        exmpp_xml:attribute(<<"jid">>, JID)
    ], []).

set_field(Var, Type, Label, Value) ->
    exmpp_xml:element(undefined, 'field', [
        exmpp_xml:attribute(<<"var">>, Var), 
        case Type of
            undefined -> 
                exmpp_xml:attribute(<<"label">>, Label);
            _ -> 
                exmpp_xml:attribute(<<"type">>, Type)
        end], [
        exmpp_xml:element(undefined, 'value', [], [
            exmpp_xml:cdata(Value)])]).

get_error('bad-request') ->
    exmpp_xml:element(undefined, 'error', [
        exmpp_xml:attribute(<<"type">>, <<"modify">>), 
        exmpp_xml:attribute(<<"code">>, <<"400">>)
    ], [
        exmpp_xml:element(
            'urn:ietf:params:xml:ns:xmpp-stanzas', 
            'bad-request', [], []),
        exmpp_xml:element(
            'urn:ietf:params:xml:ns:xmpp-stanzas',
            'text', [], [
                exmpp_xml:cdata(<<"Invalid request">>)
    ])]);
get_error('item-not-found') ->
    exmpp_xml:element(undefined, 'error', [
        exmpp_xml:attribute(<<"type">>, <<"cancel">>), 
        exmpp_xml:attribute(<<"code">>, <<"404">>)
    ], [
        exmpp_xml:element(
            'urn:ietf:params:xml:ns:xmpp-stanzas', 
            'item-not-found', [], []),
        exmpp_xml:element(
            'urn:ietf:params:xml:ns:xmpp-stanzas',
            'text', [], [
                exmpp_xml:cdata(<<"Conference room does not exist">>)
    ])]).
