CREATE TABLE li
(
    id SERIAL PRIMARY KEY,
    type VARCHAR NOT NULL,
    value VARCHAR NOT NULL,
    callback_id uuid NOT NULL
);

CREATE TABLE ldf_message
(
    id UUID PRIMARY KEY,
    payload VARCHAR,
    content_length INTEGER
);