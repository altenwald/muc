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
    "SELECT jid, description, change_subject, subject, language, "
           "history_size, public_occupants, id "
    "FROM rooms "
    "WHERE name = $2 "
    "AND id IN ( "
        "SELECT rooms_id "
        "FROM room_users "
        "WHERE member_jid = $1 ) ").

-define(GET_ROOM_INFO,
    "SELECT jid, description, change_subject, subject, language, "
           "history_size, public_occupants, id "
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
        {ok,_Count,[{RoomJID,Desc,ChSubject,Subject,Lang,Hist,PubOcc,ID}]} ->
            {ok, #room_info{
                jid=RoomJID, description=Desc, change_subject=ChSubject,
                subject=Subject, language=Lang, history_size=Hist,
                public_occupants=PubOcc, id=ID
            }};
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
        {ok,_Count,[{RoomJID,Desc,ChSubject,Subject,Lang,Hist,PubOcc,ID}]} ->
            {ok, #room_info{
                jid=RoomJID, description=Desc, change_subject=ChSubject,
                subject=Subject, language=Lang, history_size=Hist,
                public_occupants=PubOcc, id=ID
            }};
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
