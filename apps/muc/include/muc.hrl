-type jid_text() :: binary().
-type jid_node() :: binary().

-type change_subject() :: all | owner | none.

-define(DEFAULT_MAX_USERS, 30).

-record(room_info,{
    jid :: binary(),
    id :: undefined | integer(),
    description = <<>> :: binary(),
    image :: undefined | binary(),
    change_subject = owner :: change_subject(),
    subject = <<>> :: binary(),
    language = <<"en">> :: binary(),
    history_size = 50 :: non_neg_integer(),
    public_occupants = true :: boolean(),
    persistent = false :: boolean(),
    public = true :: boolean(),
    password :: undefined | binary(),
    max_users = ?DEFAULT_MAX_USERS :: pos_integer(),
    real_jids = moderators :: moderators | anyone,
    members_only = true :: boolean(),
    moderated_room = false :: boolean(),
    members_by_default = true :: boolean(),
    allow_private_messages = false :: boolean(),
    allow_private_messages_from_visitors = nobody :: nobody | moderators | anyone,
    allow_query_users = true :: boolean(),
    allow_invites = true :: boolean(),
    allow_visitors_status = false :: boolean(),
    allow_visitors_change_nickname = false :: boolean(),
    allow_visitors_voice_requests = false :: boolean(),
    voice_request_min_interval = 1800 :: non_neg_integer(),
    room_owners = [] :: [jid_text()],
    main_owner = <<>> :: binary()
}).

-type affiliation() :: owner | admin | member | none.
-type role() :: moderator | participant | visitor.

-type room_info() :: #room_info{}.

-record(room_user,{
    id :: undefined | integer(),
    jid :: binary(),
    affiliation = member :: affiliation(),
    role = participant :: role(),
    nick :: binary()
}).

-type room_user() :: #room_user{}.

-define(GET_ROOMINFO_DATA(X,Y), (X#room_info.X)).
