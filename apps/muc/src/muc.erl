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
    lager:debug("get room list for [~s]~n", [FromBin]),
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
    lager:debug("get room [~s] info for [~s]~n", [Room, FromBin]),
    %% FIXME: request this information to muc_room
    case muc_db:get_room_info(FromBin, Room) of
    {ok, RoomInfo} ->
        RoomUsersCount = muc_db:get_occupants_number(From, RoomInfo#room_info.jid),
        Query = muc_iq:get_room_info(RoomInfo, RoomUsersCount),
        Result = exmpp_iq:result(IQ, Query),
        ecomponent:send(Result, ?NS_DISCO_ITEMS, muc, false),
        ok;
    {error, notfound} ->
        lager:debug("IQ = ~p~n", [IQ]),
        Error = muc_iq:get_error('item-not-found'),
        Result = exmpp_iq:error(IQ, Error),
        ecomponent:send(Result, ?NS_DISCO_ITEMS, muc, false),
        ok
    end;

%% Request disco#items for get all users from a room:

process_iq(#params{from=From, to={Room,_,_}, iq=IQ, ns=?NS_DISCO_ITEMS, type="get"}) ->
    FromBin = exmpp_jid:bare_to_binary(exmpp_jid:make(From)),
    lager:debug("get room [~s] occupants info for [~s]~n", [Room, FromBin]),
    Occupants = muc_db:get_occupants(FromBin, Room),
    Items = lists:map(fun({Name, JID}) ->
        muc_iq:set_item(Name, JID)
    end, Occupants),
    Query = exmpp_xml:element(?NS_DISCO_ITEMS, 'query', [], Items),
    Result = exmpp_iq:result(IQ, Query),
    ecomponent:send(Result, ?NS_DISCO_ITEMS, muc, false),
    ok;

%% Request disco#info for get the user information:

process_iq(#params{from=From, to={Room,_,Nick}, iq=IQ, ns=?NS_DISCO_INFO, type="get"}) ->
    FromBin = exmpp_jid:bare_to_binary(exmpp_jid:make(From)),
    lager:debug("get room [~s] user [~s] info for [~s]~n", [Room, Nick, FromBin]),
    %% TODO: should return the detailed information about the user?
    Query = exmpp_xml:element(?NS_DISCO_ITEMS, 'query', [], []),
    Result = exmpp_iq:result(IQ, Query),
    ecomponent:send(Result, ?NS_DISCO_ITEMS, muc, false),
    ok;

%% Configure ROOM (get form):

process_iq(#params{from=From, to={_,_,Nick}=To, iq=IQ, ns=?NS_MUC_OWNER, type="get"}) ->
    FromBin = exmpp_jid:bare_to_binary(exmpp_jid:make(From)),
    RoomBin = exmpp_jid:bare_to_binary(exmpp_jid:make(To)),
    lager:debug("get room [~s] user [~s] info for [~s]~n", [RoomBin, Nick, FromBin]),
    muc_room:get_config(RoomBin, FromBin, Nick, IQ),
    ok;

%% Configure ROOM (submit):

process_iq(#params{from=From, to={_,_,Nick}=To, iq=IQ, ns=?NS_MUC_OWNER, type="set"}) ->
    FromBin = exmpp_jid:bare_to_binary(exmpp_jid:make(From)),
    RoomBin = exmpp_jid:bare_to_binary(exmpp_jid:make(To)),
    lager:debug("setting room [~s] user [~s] info for [~s]~n", [RoomBin, Nick, FromBin]),
    muc_room:set_config(RoomBin, FromBin, Nick, IQ),
    ok;

%% Bad-request IQ: 

process_iq(#params{iq=IQ}=Params) ->
    lager:error("bad-request for params: ~p~n", [Params]),
    Error = muc_iq:get_error('bad-request'),
    Result = exmpp_iq:error(IQ, Error),
    ecomponent:send(Result, ?NS_DISCO_ITEMS, muc, false),
    ok.

%% Create a room

process_presence(#presence{from=From, type="available", to={Room,_,Nick}=To, xmlel=Xmlel}) ->
    FromBin = exmpp_jid:bare_to_binary(exmpp_jid:make(From)),
    lager:debug("get room [~s] user [~s] info for ~s~n", [Room, Nick, FromBin]),
    case exmpp_xml:get_path(Xmlel, [{element, "x"}]) of
    undefined ->
        %% enter a room
        case muc_db:get_room_info(FromBin, Room) of
        {error, notfound} ->
            %% room doesn't exist
            lager:warning("trying to enter in a non-existent room! ~s~n", [Room]),
            %% TODO: presence error return (see XEP-0045)
            ok;
        #room_info{} ->
            %% enter in the room
            lager:debug("user [~s] enter in the room [~s]~n", [FromBin, Room]),
            %% TODO: enter in the room.
            ok
        end;
    #xmlel{ns = ?NS_MUC} ->
        %% create a room
        case muc_db:get_room_info(FromBin, Room) of
        {error, notfound} ->
            %% creating the room
            lager:info("creating room [~s]~n", [Room]),
            RoomJID = exmpp_jid:bare_to_binary(exmpp_jid:make(To)),
            muc_room:create_room(RoomJID, FromBin, Nick),
            ok;
        {ok, #room_info{}} ->
            %% room cannot be created
            lager:warning("room [~s] exists!~n", [Room]),
            %% TODO: send the error
            ok
        end
    end;

process_presence(Params) ->
    lager:error("bad-presence for params: ~p~n", [Params]),
    ok.

%% --------------------------------------------------------------------------
%% Internal functions:

