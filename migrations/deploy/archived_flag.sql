-- Deploy octopod:archived_flag to pg

BEGIN;

ALTER TABLE deployments ADD archived BOOLEAN NOT NULL DEFAULT 'f';
ALTER TABLE deployments ADD archived_at TIMESTAMPTZ;

CREATE INDEX deployments_archived_archived_at_idx ON deployments USING BTREE (archived, archived_at);


ALTER TABLE deployment_logs ADD archived BOOLEAN NOT NULL DEFAULT 'f';


COMMIT;

ALTER TYPE actions ADD VALUE 'delete' AFTER 'update';
ALTER TYPE actions ADD VALUE 'restore' AFTER 'delete';
