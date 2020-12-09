-- Deploy octopod:rename-delete-to-archive to pg
BEGIN;


ALTER TYPE statuses RENAME VALUE 'DeletePending' TO 'ArchivePending';


COMMIT;
