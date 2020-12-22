-- Verify octopod:metadata-ordering on pg
BEGIN;


SELECT "ord"
FROM "deployment_metadata"
WHERE FALSE;


ROLLBACK;
