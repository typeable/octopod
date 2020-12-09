-- Deploy octopod:deployments to pg

BEGIN;

CREATE TABLE deployments (
    id BIGSERIAL PRIMARY KEY,
    name TEXT NOT NULL,
    tag TEXT NOT NULL,
    envs TEXT NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE UNIQUE INDEX deployments_name_key ON deployments USING BTREE (name);

COMMIT;
