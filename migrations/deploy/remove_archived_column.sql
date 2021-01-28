-- Deploy octopod:remove_archived_column to pg
 BEGIN;


ALTER TABLE "public"."deployments"
DROP COLUMN "archived";


COMMIT;
