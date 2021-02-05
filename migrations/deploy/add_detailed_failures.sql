-- Deploy octopod:add_detailed_failures to pg
 BEGIN;


ALTER TYPE statuses RENAME VALUE 'Failure' TO 'GenericFailure';


ALTER TYPE statuses ADD VALUE IF NOT EXISTS 'TagMismatch';


ALTER TYPE statuses ADD VALUE IF NOT EXISTS 'PartialAvailability';


COMMIT;
