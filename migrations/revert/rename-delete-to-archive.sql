-- Revert octopod:rename-delete-to-archive from pg
BEGIN;


ALTER TYPE statuses RENAME VALUE 'ArchivePending' TO 'DeletePending';


COMMIT;
