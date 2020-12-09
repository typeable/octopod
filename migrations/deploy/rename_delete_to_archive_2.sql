-- Deploy octopod:rename_delete_to_archive_2 to pg

BEGIN;

ALTER TYPE actions RENAME VALUE 'delete' TO 'archive';

COMMIT;
