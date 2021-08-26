-- Verify octopod:migrate_2.0 on pg

BEGIN;

SELECT
  id,
  name,
  tag,
  app_overrides,
  deployment_overrides,
  created_at,
  updated_at,
  archived_at,
  status,
  status_updated_at,
  checked_at,
  links
FROM deployments
WHERE false;

SELECT
  id,
  deployment_id,
  action,
  tag,
  exit_code,
  created_at,
  archived,
  duration,
  stdout,
  stderr,
  app_overrides,
  deployment_overrides
FROM deployment_logs
WHERE false;

ROLLBACK;
