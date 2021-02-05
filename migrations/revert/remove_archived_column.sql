-- Revert octopod:remove_archived_column from pg
 BEGIN;


ALTER TABLE "public"."deployments" ADD COLUMN "archived" bool NOT NULL DEFAULT 'false';


COMMIT;
