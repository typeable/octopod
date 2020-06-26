-- Deploy dm:add_duration_and_stdout_and_stderr_to_deployment_logs to pg

BEGIN;

ALTER TABLE deployment_logs ADD duration INT NOT NULL DEFAULT 0;
ALTER TABLE deployment_logs ADD stdout TEXT NOT NULL DEFAULT '';
ALTER TABLE deployment_logs ADD stderr TEXT NOT NULL DEFAULT '';

COMMIT;
