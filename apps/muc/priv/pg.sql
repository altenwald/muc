
CREATE TABLE rooms (
    id SERIAL NOT NULL PRIMARY KEY,
    name VARCHAR(128) NOT NULL,
    jid VARCHAR(255) NOT NULL,
    description VARCHAR(255) NOT NULL,
    image VARCHAR(255),
    change_subject VARCHAR(10) NOT NULL DEFAULT 'owner',
    subject TEXT,
    language CHAR(2) NOT NULL DEFAULT 'en',
    history_size INTEGER NOT NULL DEFAULT 50,
    public_occupants BOOLEAN NOT NULL DEFAULT TRUE,
    public BOOLEAN NOT NULL DEFAULT TRUE,
    password VARCHAR(64) DEFAULT NULL,
    max_users INTEGER NOT NULL DEFAULT 30,
    real_jids VARCHAR(10) NOT NULL DEFAULT 'moderators',
    members_only BOOLEAN NOT NULL DEFAULT TRUE,
    moderated_room BOOLEAN NOT NULL DEFAULT FALSE,
    members_by_default BOOLEAN NOT NULL DEFAULT TRUE,
    allow_private_messages BOOLEAN NOT NULL DEFAULT FALSE,
    allow_private_messages_from_visitors VARCHAR(10) NOT NULL DEFAULT 'nobody',
    allow_query_users BOOLEAN NOT NULL DEFAULT TRUE,
    allow_invites BOOLEAN NOT NULL DEFAULT TRUE,
    allow_visitors_status BOOLEAN NOT NULL DEFAULT FALSE,
    allow_visitors_change_nickname BOOLEAN NOT NULL DEFAULT FALSE,
    allow_visitors_voice_requests BOOLEAN NOT NULL DEFAULT FALSE,
    voice_request_min_interval INTEGER NOT NULL DEFAULT 1800,
    room_owners TEXT DEFAULT NULL,
    main_owner VARCHAR(255) DEFAULT NULL,

    UNIQUE(name)
);

CREATE TABLE room_users (
    id SERIAL NOT NULL PRIMARY KEY,
    rooms_id INTEGER NOT NULL REFERENCES rooms(id)
        ON UPDATE CASCADE
        ON DELETE CASCADE,
    member_jid VARCHAR(255) NOT NULL,
    affiliation VARCHAR(10) NOT NULL DEFAULT 'member',
    role VARCHAR(10) NOT NULL DEFAULT 'occupant',
    nick VARCHAR(128) NOT NULL,

    UNIQUE (rooms_id, member_jid),
    UNIQUE (rooms_id, nick)
);

ALTER TABLE rooms OWNER TO ejabberd;
ALTER TABLE room_users OWNER TO ejabberd;
