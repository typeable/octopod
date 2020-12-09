-- Revert octopod:create_deployment_metadata from pg

BEGIN;

DROP TABLE deployment_metadata;

COMMIT;
