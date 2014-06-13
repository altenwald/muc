-module(muc_db).
-compile([warnings_as_errors]).

-export([
    list_rooms/1,
    get_room_info/2,
    get_occupants_number/2,
    get_occupants/2
]).

-define(POOLDB, muc_pool).
-define(DELAYDB, muc_pool_queue).

-include("muc.hrl").

-define(LIST_ROOMS, <<
    "SELECT name, jid "
    "FROM rooms "
    "WHERE id IN ( "
        "SELECT rooms_id "
        "FROM room_users "
        "WHERE member_jid = $1 ) "
>>).

-define(GET_ROOM_INFO, <<
    "SELECT jid, description, change_subject, subject, language, "
           "history_size, public_occupants, owner "
    "FROM rooms "
    "WHERE name = $2 "
    "AND id IN ( "
        "SELECT rooms_id "
        "FROM room_users "
        "WHERE member_jid = $1 ) "
>>).

-define(GET_OCCUPANTS_NUMBER, <<
    "SELECT COUNT(ru.*) "
    "FROM room_users ru JOIN rooms r ON ru.rooms_id = r.id "
    "WHERE r.name = $2 "
    "AND ru.member_jid = $1 "
>>).

-define(GET_OCCUPANTS, <<
    "SELECT ru.nick AS name, r.jid || '/' || ru.nick AS occupant "
    "FROM room_users ru JOIN rooms r ON ru.rooms_id = r.id "
    "WHERE r.name = $2 "
    "AND r.public_occupants = TRUE "
    "AND ru.member_jid = $1 "
>>).

-spec list_rooms(JID :: jid_text()) -> [[binary()]].

list_rooms(JID) ->
    dbi:do_query(?POOLDB, ?LIST_ROOMS, [JID]).

-spec get_room_info(JID :: jid_text(), Room :: jid_node()) -> 
    {ok, room_info()} | {error, Reason :: atom()}.

get_room_info(JID, Room) ->
    case catch dbi:do_query(?POOLDB, ?GET_ROOM_INFO, [JID, Room]) of
        {ok,1,[{RoomJID,Desc,ChSubject,Subject,Lang,Hist,PubOcc,Owner}]} ->
            {ok, #room_info{
                jid=RoomJID, description=Desc, change_subject=ChSubject,
                subject=Subject, language=Lang, history_size=Hist,
                public_occupants=PubOcc, owner=Owner
            }};
        {ok,0,[]} ->
            {error, notfound};
        Error ->
            lager:error("Unknown database return: ~p~n", [Error]),
            throw(ebadreturn)
    end.

-spec get_occupants_number(JID :: jid_text(), Room :: jid_node()) -> non_neg_integer().

get_occupants_number(JID, Room) ->
    case catch dbi:do_query(?POOLDB, ?GET_OCCUPANTS_NUMBER, [JID, Room]) of
        {ok,1,[{Number}]} when is_number(Number) -> 
            Number;
        Error ->
            lager:error("Unknown database return: ~p~n", [Error]),
            throw(ebadreturn)
    end.

-spec get_occupants(JID :: jid_text(), Room :: jid_node()) -> [any()].

get_occupants(JID, Room) ->
    case catch dbi:do_query(?POOLDB, ?GET_OCCUPANTS, [JID, Room]) of
        {ok,_Count,Occupants} when is_list(Occupants) ->
            Occupants;
        Error ->
            lager:error("Unknown database return: ~p~n", [Error]),
            throw(ebadreturn)
    end.
