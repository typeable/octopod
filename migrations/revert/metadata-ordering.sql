-- Revert octopod:metadata-ordering from pg
BEGIN;


DROP INDEX "deployment_metadata_ord";


ALTER TABLE "deployment_metadata"
DROP COLUMN "ord";


COMMIT;
