-module(muc_room).
-compile([warnings_as_errors]).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

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
    start_link/3
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
    case nprocreg:get_pid(JID, {muc_room, start_link, [JID,Owner,Nick]}) of
        undefined ->
            lager:error("Cannot create the room ~s~n", [JID]),
            undefined;
        PID ->
            PID
    end.

start_link(JID, Owner, Nick) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [JID,Owner,Nick], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([JIDbin,Owner,Nick]) ->
    JID = exmpp_jid:parse(JIDbin),
    OwnerUser = #room_user{
        jid = Owner,
        affiliation = owner,
        role = moderator,
        nick = Nick
    },
    muc_db:save_room(JID),
    muc_db:save_user(JID, OwnerUser),
    {ok, #state{
        jid = JID,
        room_info = #room_info{
            jid = JIDbin
        },
        users = [OwnerUser]
    }}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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

