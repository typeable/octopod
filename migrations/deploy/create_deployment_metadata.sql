-- Deploy octopod:create_deployment_metadata to pg

BEGIN;

CREATE TABLE deployment_metadata (
    id BIGSERIAL PRIMARY KEY,
    deployment_id BIGINT NOT NULL REFERENCES deployments(id),
    key TEXT NOT NULL,
    value TEXT NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE UNIQUE INDEX deployment_metadata_deployment_id_key_key ON deployment_metadata USING BTREE (deployment_id, key);

COMMIT;
