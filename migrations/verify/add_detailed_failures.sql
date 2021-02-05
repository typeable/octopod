-- Verify octopod:add_detailed_failures on pg
 BEGIN;


SELECT ('GenericFailure'::statuses,
        'TagMismatch'::statuses,
        'PartialAvailability'::statuses);


ROLLBACK;
