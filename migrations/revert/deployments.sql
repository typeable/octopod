-- Revert octopod:deployments from pg

BEGIN;

DROP TABLE deployments;

COMMIT;
