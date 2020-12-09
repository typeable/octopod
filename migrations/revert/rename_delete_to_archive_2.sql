-- Revert octopod:rename_delete_to_archive_2 from pg

BEGIN;

ALTER TYPE actions RENAME VALUE 'archive' TO 'delete';

COMMIT;
