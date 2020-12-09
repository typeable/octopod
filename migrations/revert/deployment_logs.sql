-- Revert octopod:deployment_logs from pg

BEGIN;

DROP TABLE deployment_logs;

DROP TYPE actions;

COMMIT;
