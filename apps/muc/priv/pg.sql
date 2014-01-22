
CREATE TABLE rooms (
    id SERIAL NOT NULL PRIMARY KEY,
    name VARCHAR(128) NOT NULL,
    jid VARCHAR(255) NOT NULL,
    description VARCHAR(255) NOT NULL,
    change_subject VARCHAR(10) NOT NULL DEFAULT 'owner',
    subject TEXT,
    language CHAR(2) NOT NULL DEFAULT 'en',
    history_size INTEGER NOT NULL DEFAULT 50,
    public_occupants BOOLEAN NOT NULL DEFAULT TRUE
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
    UNIQUE (nick)
);

ALTER TABLE rooms OWNER TO ejabberd;
ALTER TABLE room_users OWNER TO ejabberd;
