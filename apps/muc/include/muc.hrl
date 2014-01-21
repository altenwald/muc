-type jid_text() :: binary().
-type jid_node() :: binary().

-record(room_info,{
    jid :: binary(),
    description :: binary(),
    change_subject = owner :: all | owner | none,
    subject :: binary(),
    language = <<"en">> :: binary(),
    history_size = 50 :: non_neg_integer(),
    public_occupants = true :: boolean(),
    owner :: jid_text()
}).

-type room_info() :: #room_info{}.
