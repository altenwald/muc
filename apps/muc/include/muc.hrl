-type jid_text() :: binary().
-type jid_node() :: binary().

-type change_subject() :: all | owner | none.

-record(room_info,{
    jid :: binary(),
    description = <<>> :: binary(),
    change_subject = owner :: change_subject(),
    subject = <<>> :: binary(),
    language = <<"en">> :: binary(),
    history_size = 50 :: non_neg_integer(),
    public_occupants = true :: boolean()
}).

-type affiliation() :: owner | admin | member | none.
-type role() :: moderator | participant | visitor.

-type room_info() :: #room_info{}.

-record(room_user,{
    jid :: binary(),
    affiliation = member :: affiliation(),
    role = participant :: role(),
    nick :: binary()
}).

-type room_user() :: #room_user{}.
