CREATE TABLE li
(
    id SERIAL PRIMARY KEY,
    type VARCHAR NOT NULL,
    value VARCHAR NOT NULL,
    callback_id uuid NOT NULL
);

CREATE TABLE ldf_message
(
    id SERIAL PRIMARY KEY,
    payload VARCHAR
);