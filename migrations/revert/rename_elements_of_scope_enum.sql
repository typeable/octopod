-- Revert octopod:rename_elements_of_scope_enum from pg

BEGIN;

ALTER TYPE scopes RENAME VALUE 'ApplicationScope' to 'App';
ALTER TYPE scopes RENAME VALUE 'DeploymentScope' to 'Staging';

COMMIT;
