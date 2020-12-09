-- Deploy octopod:add_archived_status to pg

BEGIN;

COMMIT;

ALTER TYPE statuses ADD VALUE 'Archived' AFTER 'DeletePending';
