-- Deploy dm:add_deployment_overrides_and_deployment_log_overrides to pg

BEGIN;

CREATE TYPE scopes AS ENUM ('App', 'Staging');
CREATE TYPE visibilities AS ENUM ('Private', 'Public');

CREATE TABLE deployment_overrides (
    id BIGSERIAL PRIMARY KEY,
    key TEXT NOT NULL,
    value TEXT NOT NULL,
    deployment_id BIGINT REFERENCES deployments(id),
    scope scopes NOT NULL,
    visibility visibilities NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
CREATE UNIQUE INDEX deployment_overrides_key_deployment_id_key ON deployment_overrides USING BTREE (key, deployment_id, scope);
CREATE INDEX deployment_overrides_deployment_id_idx ON deployment_overrides USING BTREE (deployment_id);

CREATE TABLE deployment_log_overrides (
    id BIGSERIAL PRIMARY KEY,
    key TEXT NOT NULL,
    value TEXT NOT NULL,
    deployment_log_id BIGINT REFERENCES deployment_logs(id),
    scope scopes NOT NULL,
    visibility visibilities NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
CREATE UNIQUE INDEX deployment_log_overrides_key_deployment_log_id_key ON deployment_log_overrides USING BTREE (key, deployment_log_id, scope);
CREATE INDEX deployment_log_overrides_deployment_log_id_idx ON deployment_log_overrides USING BTREE (deployment_log_id);

DO LANGUAGE 'plpgsql' $$
DECLARE
    _r  RECORD;
    _r2 RECORD;
    _k  TEXT;
    _v  TEXT;
BEGIN
    FOR _r IN SELECT id, envs FROM deployments LOOP
        FOR _r2 IN SELECT regexp_split_to_table(_r.envs, '\n') as env LOOP
            _k = split_part(_r2.env, '=', 1);
            _v = split_part(_r2.env, '=', 2);
            IF _k != '' THEN
                INSERT INTO deployment_overrides (key, value, deployment_id, scope, visibility)
                VALUES (_k, _v, _r.id, 'App', 'Public')
                ON CONFLICT (key, deployment_id, scope)
                DO
                    UPDATE SET value = _v, updated_at = now();
            END IF;
        END LOOP;
    END LOOP;
END
$$;

DO LANGUAGE 'plpgsql' $$
DECLARE
    _r  RECORD;
    _r2 RECORD;
    _k  TEXT;
    _v  TEXT;
BEGIN
    FOR _r IN SELECT id, envs FROM deployment_logs LOOP
        FOR _r2 IN SELECT regexp_split_to_table(_r.envs, '\n') as env LOOP
            _k = split_part(_r2.env, '=', 1);
            _v = split_part(_r2.env, '=', 2);
            IF _k != '' THEN
                INSERT INTO deployment_log_overrides (key, value, deployment_log_id, scope, visibility)
                VALUES (_k, _v, _r.id, 'App', 'Public')
                ON CONFLICT (key, deployment_log_id, scope)
                DO
                    UPDATE SET value = _v, updated_at = now();
            END IF;
        END LOOP;
    END LOOP;
END
$$;

ALTER TABLE deployments DROP envs;
ALTER TABLE deployment_logs DROP envs;

COMMIT;
