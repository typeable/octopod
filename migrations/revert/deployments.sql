-- Revert dm:deployments from pg

BEGIN;

DROP TABLE deployments;

COMMIT;
