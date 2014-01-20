-module(muc_db).

-export([list_rooms/1]).

-define(POOL, ejabberd).

-define(LIST_ROOMS, 
    "SELECT name, jid "
    "FROM rooms "
    "WHERE id IN ( "
        "SELECT rooms_id "
        "FROM room_users "
        "WHERE member_jid = ? ) ").

list_rooms(JID) ->
    {ok, C} = epgsql_pool:get_connection(?POOL),
    {ok, _Columns, Rows} = pgsql:equery(C, ?LIST_ROOMS, [JID]),
    epgsql_pool:return_connection(?POOL, C),
    Rows.
