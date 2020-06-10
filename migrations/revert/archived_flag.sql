-- Revert dm:archived_flag from pg

BEGIN;

ALTER TABLE deployments DROP archived;
ALTER TABLE deployments DROP archived_at;

ALTER TABLE deployment_logs DROP archived;

COMMIT;
