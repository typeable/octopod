-- Revert octopod:add_duration_and_stdout_and_stderr_to_deployment_logs from pg

BEGIN;

ALTER TABLE deployment_logs DROP duration;
ALTER TABLE deployment_logs DROP stdout;
ALTER TABLE deployment_logs DROP stderr;

COMMIT;
