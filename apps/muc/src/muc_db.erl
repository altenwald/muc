-module(muc_db).
-compile([warnings_as_errors]).

-export([
    list_rooms/1,
    get_room_info/1,
    get_room_info/2,
    get_occupants_number/2,
    get_occupants/2,
    save_room/3,
    save_user/2,
    get_users/1,
    update_room/1,

    to_role/1,
    to_affiliation/1,
    from_role/1,
    from_affiliation/1
]).

-define(POOL, ejabberd).

-include("muc.hrl").

-define(LIST_ROOMS, 
    "SELECT name, jid "
    "FROM rooms "
    "WHERE id IN ( "
        "SELECT rooms_id "
        "FROM room_users "
        "WHERE member_jid = $1 ) ").

-define(GET_USERS,
    "SELECT id, member_jid, affiliation, role, nick "
    "FROM room_users "
    "WHERE rooms_id IN "
        "(SELECT id FROM rooms WHERE jid = $1)").

-define(GET_ROOM_INFO_SEC,
    "SELECT id, description, image, change_subject, subject,"
          " language, history_size, public_occupants,"
          " public, password, max_users, real_jids,"
          " members_only, moderated_room, members_by_default,"
          " allow_private_messages, allow_private_messages_from_visitors,"
          " allow_query_users, allow_invites, allow_visitors_status,"
          " allow_visitors_change_nickname, allow_visitors_voice_requests,"
          " voice_request_min_interval, main_owner, jid, room_owners "
    "FROM rooms "
    "WHERE name = $2 "
    "AND id IN ( "
        "SELECT rooms_id "
        "FROM room_users "
        "WHERE member_jid = $1 ) ").

-define(GET_ROOM_INFO,
    "SELECT id, description, image, change_subject, subject,"
          " language, history_size, public_occupants,"
          " public, password, max_users, real_jids,"
          " members_only, moderated_room, members_by_default,"
          " allow_private_messages, allow_private_messages_from_visitors,"
          " allow_query_users, allow_invites, allow_visitors_status,"
          " allow_visitors_change_nickname, allow_visitors_voice_requests,"
          " voice_request_min_interval, main_owner, jid, room_owners "
    "FROM rooms "
    "WHERE name = $1 ").

-define(GET_OCCUPANTS_NUMBER,
    "SELECT COUNT(ru.*) "
    "FROM room_users ru JOIN rooms r ON ru.rooms_id = r.id "
    "WHERE r.name = $2 "
    "AND ru.member_jid = $1 ").

-define(GET_OCCUPANTS,
    "SELECT ru.nick AS name, r.jid || '/' || ru.nick AS occupant "
    "FROM room_users ru JOIN rooms r ON ru.rooms_id = r.id "
    "WHERE r.name = $2 "
    "AND r.public_occupants = TRUE "
    "AND ru.member_jid = $1 ").

-define(SAVE_ROOM,
    "INSERT INTO rooms(name, jid, description, subject) "
    "VALUES( $1, $2, $3, $4 ) "
    "RETURNING id ").

-define(UPDATE_ROOM,
    "UPDATE rooms "
    "SET description = $2, image = $3, change_subject = $4, subject = $5,"
       " language = $6, history_size = $7, public_occupants = $8,"
       " public = $9, password = $10, max_users = $11, real_jids = $12,"
       " members_only = $13, moderated_room = $14, members_by_default = $15,"
       " allow_private_messages = $16, allow_private_messages_from_visitors = $17,"
       " allow_query_users = $18, allow_invites = $19, allow_visitors_status = $20,"
       " allow_visitors_change_nickname = $21, allow_visitors_voice_requests = $22,"
       " voice_request_min_interval = $23, main_owner = $24, room_owners = $25 "
    "WHERE id = $1 ").

-define(SAVE_USER,
    "INSERT INTO room_users(rooms_id, member_jid, affiliation, role, nick) "
    "VALUES ( ("
        "SELECT id FROM rooms WHERE jid = $1 ORDER BY id DESC LIMIT 1"
        "), $2, $3, $4, $5 ) "
    "RETURNING id ").

-spec list_rooms(JID :: jid_text()) -> [[binary()]].

list_rooms(JID) ->
    {ok,_Count,Rows} = pg_query(?LIST_ROOMS, [JID]),
    Rows.

-spec get_room_info(JID :: jid_text(), Room :: jid_node()) -> 
    {ok, room_info()} | {error, Reason :: atom()}.

get_room_info(JID, Room) ->
    case pg_query(?GET_ROOM_INFO_SEC, [JID, Room]) of
        {ok,_Count,[Row]} ->
            {ok, room_fields_map(Row)};
        {ok,0,[]} ->
            {error, notfound};
        {error,Error} ->
            lager:error("Unknown database return: ~p~n", [Error]),
            throw(ebadreturn)
    end.

-spec get_room_info(Room :: jid_node()) -> 
    {ok, room_info()} | {error, Reason :: atom()}.

get_room_info(Room) ->
    case pg_query(?GET_ROOM_INFO, [Room]) of
        {ok,_Count,[Row]} ->
            {ok, room_fields_map(Row)};
        {ok,0,[]} ->
            {error, notfound};
        {error,Error} ->
            lager:error("Unknown database return: ~p~n", [Error]),
            throw(ebadreturn)
    end.

-spec get_occupants_number(JID :: jid_text(), Room :: jid_node()) -> non_neg_integer().

get_occupants_number(JID, Room) ->
    case pg_query(?GET_OCCUPANTS_NUMBER, [JID, Room]) of
        {ok,_Count,[{Number}]} when is_number(Number) -> 
            Number;
        {error,Error} ->
            lager:error("Unknown database return: ~p~n", [Error]),
            throw(ebadreturn)
    end.

-spec get_occupants(JID :: jid_text(), Room :: jid_node()) -> [any()].

get_occupants(JID, Room) ->
    case pg_query(?GET_OCCUPANTS, [JID, Room]) of
        {ok,_Count,Occupants} ->
            Occupants;
        {error,Error} ->
            lager:error("Unknown database return: ~p~n", [Error]),
            throw(ebadreturn)
    end.

-spec save_room(JID :: jid_text(), Description :: binary(), Subject :: binary()) -> 
    {ok, RoomInfo :: room_info()} | {error, any()}.

save_room(JID, Description, Subject) ->
    Name = exmpp_jid:node(exmpp_jid:parse(JID)),
    case pg_query(?SAVE_ROOM, [Name, JID, Description, Subject]) of
        {ok,1,[{Id}]} -> 
            {ok, #room_info{
                id=Id, jid=JID, description=Description,
                subject=Subject}};
        {error, Error} ->
            lager:error("Cannot save the room [~s]: ~p~n", [JID, Error]),
            {error, Error}
    end.

-spec save_user(JID :: jid_text(), User :: room_user()) -> 
    {ok, room_user()} | {error, any()}.

save_user(JID, #room_user{
        jid=User, affiliation=Affiliation, role=Role, 
        nick=Nick}=RoomUser) ->
    A = from_affiliation(Affiliation),
    R = from_role(Role),
    case pg_query(?SAVE_USER, [JID, User, A, R, Nick]) of
        {ok,1,[{Id}]} ->
            {ok, RoomUser#room_user{id=Id}};
        {error, Error} ->
            lager:error("Cannot save the user [~s] in the room [~s]: ~p~n", 
                [User, JID, Error]),
            {error, Error}
    end.

-spec get_users(Room :: jid_text()) ->
    {ok, [room_user()]} | {error, any()}.

get_users(Room) ->
    case pg_query(?GET_USERS, [Room]) of
        {ok,_Count,Users} ->
            ParsedUsers = lists:map(fun({ID,JID,A,R,Nick}) ->
                #room_user{
                    id=ID, jid=JID, affiliation=to_affiliation(A),
                    role=to_role(R), nick=Nick
                }
            end, Users),
            {ok, ParsedUsers};
        {error, Error} ->
            lager:error("Cannot get users list for room [~s]~n", [Error])
    end.

-spec update_room(RoomInfo :: room_info()) -> 
    {ok, RoomInfo :: room_info()} | {error, any()}.

update_room(RoomInfo) ->
    Params = [
        RoomInfo#room_info.id,
        RoomInfo#room_info.description,
        RoomInfo#room_info.image,
        RoomInfo#room_info.change_subject,
        RoomInfo#room_info.subject,
        RoomInfo#room_info.language,
        RoomInfo#room_info.history_size,
        RoomInfo#room_info.public_occupants,
        RoomInfo#room_info.public,
        RoomInfo#room_info.password,
        RoomInfo#room_info.max_users,
        RoomInfo#room_info.real_jids,
        RoomInfo#room_info.members_only,
        RoomInfo#room_info.moderated_room,
        RoomInfo#room_info.members_by_default,
        RoomInfo#room_info.allow_private_messages,
        RoomInfo#room_info.allow_private_messages_from_visitors,
        RoomInfo#room_info.allow_query_users,
        RoomInfo#room_info.allow_invites,
        RoomInfo#room_info.allow_visitors_status,
        RoomInfo#room_info.allow_visitors_change_nickname,
        RoomInfo#room_info.allow_visitors_voice_requests,
        RoomInfo#room_info.voice_request_min_interval,
        RoomInfo#room_info.main_owner,
        case RoomInfo#room_info.room_owners of
            [] -> null;
            [RoomOwner|RoomOwners] ->
                lists:foldl(fun(RO, Result) ->
                    <<Result/binary, ";", RO/binary>>
                end, RoomOwner, RoomOwners)
        end
    ],
    case pg_query(?UPDATE_ROOM, Params) of
        {ok,1,_} -> {ok, RoomInfo};
        {error,Reason} -> {error, Reason}
    end.

%% -----------------------------------------------------------------------
%% Internal functions

-spec pg_query(SQL :: binary() | string(), [Params :: any()]) -> [string() | binary()].

pg_query(SQL, Params) ->
    {ok, C} = pgsql_pool:get_connection(?POOL),
    lager:debug("executing query [~s] with params ~p~n", [SQL, Params]),
    Result = case pgsql:equery(C, SQL, Params) of
        {ok, _Columns, Rows} -> {ok, length(Rows), Rows};
        {ok, Count} -> {ok, Count, []};
        {ok, Count, _Columns, Rows} -> {ok, Count, Rows};
        {error, Error} -> {error, Error}
    end,
    pgsql_pool:return_connection(?POOL, C),
    Result.

-spec values(null | binary()) -> [binary()].

values(null) -> [];
values(Values) -> binary:split(Values, <<";">>, [global]).

-spec string(null | binary()) -> undefined | binary().

string(null) -> undefined;
string(Any) -> Any.

-spec to_role(Role :: binary()) -> role().

to_role(<<"moderator">>) -> moderator;
to_role(<<"participant">>) -> participant;
to_role(<<"visitor">>) -> visitor.

-spec to_affiliation(Affiliation :: binary()) -> affiliation().

to_affiliation(<<"owner">>) -> owner;
to_affiliation(<<"admin">>) -> admin;
to_affiliation(<<"member">>) -> member;
to_affiliation(<<"none">>) -> none.

-spec from_role(Role :: role()) -> binary().

from_role(moderator) -> <<"moderator">>;
from_role(participant) -> <<"participant">>;
from_role(visitor) -> <<"visitor">>.

-spec from_affiliation(Affiliation :: affiliation()) -> binary().

from_affiliation(owner) -> <<"owner">>;
from_affiliation(admin) -> <<"admin">>;
from_affiliation(member) -> <<"member">>;
from_affiliation(none) -> <<"none">>.

-spec room_fields_map(Row :: [any()]) -> room_info().

room_fields_map(Row) ->
    #room_info{
        id = element(1, Row),
        description = string(element(2, Row)),
        image = string(element(3, Row)),
        change_subject = 
            binary_to_existing_atom(element(4, Row), utf8),
        subject = string(element(5, Row)),
        language = element(6, Row),
        history_size = element(7, Row),
        public_occupants = element(8, Row),
        public = element(9, Row),
        password = string(element(10, Row)),
        max_users = element(11, Row),
        real_jids = binary_to_existing_atom(element(12, Row), utf8),
        members_only = element(13, Row),
        moderated_room = element(14, Row),
        members_by_default = element(15, Row),
        allow_private_messages = element(16, Row),
        allow_private_messages_from_visitors = 
            binary_to_existing_atom(element(17, Row), utf8),
        allow_query_users = element(18, Row),
        allow_invites = element(19, Row),
        allow_visitors_status = element(20, Row),
        allow_visitors_change_nickname = element(21, Row),
        allow_visitors_voice_requests = element(22, Row),
        voice_request_min_interval = element(23, Row),
        main_owner = string(element(24, Row)),
        jid = element(25, Row),
        room_owners = values(element(26, Row))
    }.
