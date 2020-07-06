-- Deploy dm:status_and_status_updated_at to pg

BEGIN;

CREATE TYPE statuses as ENUM ('Running', 'Failure', 'CreatePending', 'UpdatePending', 'DeletePending');

ALTER TABLE deployments ADD status statuses NOT NULL DEFAULT 'Running';
ALTER TABLE deployments ADD status_updated_at TIMESTAMPTZ NOT NULL DEFAULT now();
ALTER TABLE deployments ADD checked_at TIMESTAMPTZ NOT NULL DEFAULT now();

CREATE INDEX deployments_checked_at_idx ON deployments USING BTREE (checked_at);

COMMIT;
