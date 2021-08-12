-- Revert octopod:migrate_2.0 from pg

BEGIN;

DO LANGUAGE 'plpgsql'
$$
BEGIN
RAISE EXCEPTION 'Revert not supported for this migration';
END
$$;

COMMIT;
