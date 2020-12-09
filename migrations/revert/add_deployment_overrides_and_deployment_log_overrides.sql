-- Revert octopod:add_deployment_overrides_and_deployment_log_overrides from pg

BEGIN;

ALTER TABLE deployments ADD envs TEXT NOT NULL DEFAULT '';
ALTER TABLE deployment_logs ADD envs TEXT NOT NULL DEFAULT '';

DO LANGUAGE 'plpgsql' $$
DECLARE
    _r   RECORD;
    _r2  RECORD;
    _buf TEXT;
BEGIN
    FOR _r IN SELECT DISTINCT deployment_id AS id FROM deployment_overrides LOOP
        _buf = '';
        FOR _r2 IN SELECT key || '=' || value AS env FROM deployment_overrides WHERE deployment_id = _r.id ORDER BY id LOOP
            _buf = _buf || _r2.env || E'\n';
        END LOOP;
        UPDATE deployments SET envs = rtrim(_buf, E'\n') WHERE id = _r.id;
    END LOOP;
END
$$;

DO LANGUAGE 'plpgsql' $$
DECLARE
    _r   RECORD;
    _r2  RECORD;
    _buf TEXT;
BEGIN
    FOR _r IN SELECT DISTINCT deployment_log_id AS id FROM deployment_log_overrides LOOP
        _buf = '';
        FOR _r2 IN SELECT key || '=' || value AS env FROM deployment_log_overrides WHERE deployment_log_id = _r.id ORDER BY id LOOP
            _buf = _buf || _r2.env || E'\n';
        END LOOP;
        UPDATE deployment_logs SET envs = rtrim(_buf, E'\n') WHERE id = _r.id;
    END LOOP;
END
$$;

DROP TABLE deployment_overrides;
DROP TABLE deployment_log_overrides;

DROP TYPE scopes;
DROP TYPE visibilities;

COMMIT;
