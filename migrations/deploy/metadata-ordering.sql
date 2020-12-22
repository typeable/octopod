-- Deploy octopod:metadata-ordering to pg
BEGIN;


ALTER TABLE "deployment_metadata" ADD COLUMN "ord" int NOT NULL DEFAULT '1';


ALTER TABLE "deployment_metadata"
ALTER COLUMN "ord"
DROP DEFAULT;


CREATE INDEX "deployment_metadata_ord" ON "deployment_metadata"("ord");


COMMIT;
