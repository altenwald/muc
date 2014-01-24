-module(muc_room).
-compile([warnings_as_errors]).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-include_lib("exmpp/include/exmpp.hrl").
-include("muc.hrl").

-record(state, {
    jid :: exmpp_jid:jid(),
    room_info :: room_info(),
    users :: [room_user()]
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    create_room/3,
    get_config/4,
    set_config/4,

    start_link/4
]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

create_room(JID, Owner, Nick) ->
    lager:debug("creating JID [~s]~n", [JID]),
    case nprocreg:get_pid(JID, {muc_room, start_link, [create,JID,Owner,Nick]}) of
        undefined ->
            lager:error("Cannot create the room ~s~n", [JID]),
            undefined;
        PID ->
            PID
    end.

get_config(JID, From, Nick, IQ) ->
    lager:debug("getting config for [~s] from [~s]~n", [JID, From]),
    case nprocreg:get_pid(JID, {muc_room, start_link, [up,JID,From,Nick]}) of
        undefined ->
            lager:error("Cannot configure the room [~s]~n", [JID]),
            throw(enoroom);
        PID ->
            gen_server:cast(PID, {get_config, From, Nick, IQ})
    end.

set_config(JID, From, Nick, IQ) ->
    lager:debug("setting config for [~s] from [~s]~n", [JID, From]),
    case nprocreg:get_pid(JID, {muc_room, start_link, [up,JID,From,Nick]}) of
        undefined ->
            lager:error("Cannot configure the room [~s]~n", [JID]),
            throw(enoroom);
        PID ->
            gen_server:cast(PID, {set_config, From, Nick, IQ})
    end.

start_link(Type, JID, Owner, Nick) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Type,JID,Owner,Nick], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([up,JIDbin,_User,_Nick]) ->
    JID = exmpp_jid:parse(JIDbin),
    {ok, RoomInfo} = muc_db:get_room_info(exmpp_jid:node(JID)),
    {ok, RoomUsers} = muc_db:get_users(JIDbin),
    {ok, #state{
        jid = JID,
        room_info = RoomInfo,
        users = RoomUsers
    }};

init([create,JIDbin,Owner,Nick]) ->
    JID = exmpp_jid:parse(JIDbin),
    OwnerUser = #room_user{
        jid = Owner,
        affiliation = owner,
        role = moderator,
        nick = Nick
    },
    {ok, RoomInfo} = muc_db:save_room(JIDbin, '', ''),
    {ok, RoomOwner} = muc_db:save_user(JIDbin, OwnerUser),
    Reply = muc_presence:create_room(JIDbin, Nick, Owner),
    ecomponent:send_presence(Reply),
    {ok, #state{
        jid = JID,
        room_info = RoomInfo,
        users = [RoomOwner]
    }}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({get_config, From, _Nick, IQ}, #state{
        jid=JID, room_info=RoomInfo, users=Users}=State) ->
    lager:debug("sending config to ~p from room [~s]~n", 
        [From, exmpp_jid:node(JID)]),
    Query = muc_iq:get_room_config(RoomInfo, Users),
    Result = exmpp_iq:result(IQ, Query),
    ecomponent:send(Result, ?NS_MUC_OWNER, muc, false),
    {noreply, State};

handle_cast({set_config, From, _Nick, IQ}, #state{
        jid=JID, room_info=RoomInfo}=State) ->
    lager:debug("setting config from ~p for room [~s]~n", 
        [From, exmpp_jid:node(JID)]),
    try
        NewRoomInfo = lists:foldl(fun
            ({<<"muc#roomconfig_roomname">>,Val},R) ->
                R#room_info{subject=Val};
            ({<<"muc#roomconfig_roomdesc">>,Val},R) ->
                R#room_info{description=Val};
            ({<<"muc#roomconfig_roomimage">>,Val},R) ->
                R#room_info{image=Val};
            ({<<"muc#roomconfig_persistentroom">>,Val},R) ->
                R#room_info{persistent=bin_to_bol(Val)};
            ({<<"muc#roomconfig_publicroom">>,Val},R) ->
                R#room_info{public=bin_to_bol(Val)};
            ({<<"muc#roomconfig_passwordprotectedroom">>,<<"0">>},R) ->
                R#room_info{password=undefined};
            ({<<"muc#roomconfig_passwordprotectedroom">>,<<"1">>},RInfo) ->
                RInfo;
            ({<<"muc#roomconfig_maxusers">>,Val},R) ->
                R#room_info{max_users=bin_to_int(Val)};
            ({<<"muc#roomconfig_changesubject">>,Val},R) ->
                R#room_info{change_subject=bin_to_bol(Val)};
            ({<<"muc#roomconfig_membersonly">>,Val},R) ->
                R#room_info{members_only=bin_to_bol(Val)};
            ({<<"allow_query_users">>,Val},R) ->
                R#room_info{allow_query_users=bin_to_bol(Val)};
            ({<<"muc#roomconfig_allowinvites">>,Val},R) ->
                R#room_info{allow_invites=bin_to_bol(Val)};
            ({<<"muc#roomconfig_mainowner">>,Val},R) ->
                R#room_info{main_owner=Val};
            %% TODO: include the rest of the params
            ({<<"FROM_TYPE">>,_},RInfo) ->
                RInfo;
            ({Var,Val},RInfo) -> lager:warning(
                "Unknown [~s] param configurated as [~s]~n", [Var,Val]),
                RInfo
        end, RoomInfo, muc_iq:get_fields(IQ)),
        {ok,NewRoomInfo} = muc_db:update_room(NewRoomInfo),
        OkResult = exmpp_iq:result(IQ),
        ecomponent:send(OkResult, ?NS_MUC_OWNER, muc, false),
        {noreply, State#state{room_info=NewRoomInfo}}
    catch Error ->
        lager:warning("Data in the form invalid = ~p~n", [Error]),
        Error = muc_iq:get_error('not-acceptable'),
        ErrorResult = exmpp_iq:error(IQ, Error),
        ecomponent:send(ErrorResult, ?NS_MUC_OWNER, muc, false),
        {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

bin_to_bol(<<"0">>) -> false;
bin_to_bol(<<"1">>) -> true.

bin_to_int(Bin) -> list_to_integer(binary_to_list(Bin)).
