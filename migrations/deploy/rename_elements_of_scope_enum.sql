-- Deploy octopod:rename_elements_of_scope_enum to pg

BEGIN;

ALTER TYPE scopes RENAME VALUE 'App' to 'ApplicationScope';
ALTER TYPE scopes RENAME VALUE 'Staging' to 'DeploymentScope';

COMMIT;
