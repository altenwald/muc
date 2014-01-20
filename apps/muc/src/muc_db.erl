-module(muc_db).
-compile([warnings_as_errors]).

-export([list_rooms/1]).

-define(POOL, ejabberd).

-define(LIST_ROOMS, 
    "SELECT name, jid "
    "FROM rooms "
    "WHERE id IN ( "
        "SELECT rooms_id "
        "FROM room_users "
        "WHERE member_jid = $1 ) ").

list_rooms(JID) ->
    {ok, C} = pgsql_pool:get_connection(?POOL),
    {ok, _Columns, Rows} = pgsql:equery(C, ?LIST_ROOMS, [JID]),
    pgsql_pool:return_connection(?POOL, C),
    Rows.
