-- Revert octopod:status_and_status_updated_at from pg

BEGIN;

ALTER TABLE deployments DROP status;
ALTER TABLE deployments DROP status_updated_at;
ALTER TABLE deployments DROP checked_at;

DROP TYPE statuses;

COMMIT;
