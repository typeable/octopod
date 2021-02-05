-- Revert octopod:add_detailed_failures from pg
 BEGIN;


ALTER TYPE statuses RENAME VALUE 'GenericFailure' TO 'Failure';

-- NOTE: not a full revert. Removing enum values is a pain.

COMMIT;
