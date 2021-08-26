-- Deploy octopod:migrate_2.0 to pg

BEGIN;

-- XXX Add DDLs here.

ALTER TABLE deployments ADD COLUMN app_overrides jsonb;
UPDATE deployments AS d SET app_overrides =
  ( SELECT COALESCE(jsonb_agg(jsonb_build_array("do".key, jsonb_build_object('tag', 'ValueAdded', 'contents', "do".value))), jsonb_build_array())
    FROM deployment_overrides as "do"
    WHERE d.id = "do".deployment_id AND scope = 'ApplicationScope'
  );
ALTER TABLE deployments ALTER COLUMN app_overrides SET NOT NULL;

ALTER TABLE deployments ADD COLUMN deployment_overrides jsonb;
UPDATE deployments AS d SET deployment_overrides =
  ( SELECT COALESCE(jsonb_agg(jsonb_build_array("do".key, jsonb_build_object('tag', 'ValueAdded', 'contents', "do".value))), jsonb_build_array())
    FROM deployment_overrides as "do"
    WHERE d.id = "do".deployment_id AND scope = 'DeploymentScope'
  );
ALTER TABLE deployments ALTER COLUMN deployment_overrides SET NOT NULL;

ALTER TABLE deployments ADD COLUMN links jsonb;
UPDATE deployments AS d SET links =
  ( SELECT COALESCE(jsonb_agg(jsonb_build_object('name', "dm".key, 'link', "dm".value)), jsonb_build_array())
    FROM deployment_metadata as "dm"
    WHERE d.id = "dm".deployment_id
  );
ALTER TABLE deployments ALTER COLUMN links SET NOT NULL;

ALTER TABLE deployments ALTER COLUMN "status" DROP DEFAULT;
ALTER TABLE deployments ALTER COLUMN "status" TYPE text;

DROP TABLE deployment_overrides;
DROP TABLE deployment_metadata;


ALTER TABLE deployment_logs ALTER COLUMN "action" TYPE text;

ALTER TABLE deployment_logs ALTER COLUMN "duration" DROP DEFAULT;
ALTER TABLE deployment_logs ALTER COLUMN "duration" TYPE interval USING (duration || ' seconds') :: interval;

ALTER TABLE deployment_logs ADD COLUMN deployment_overrides jsonb;
UPDATE deployment_logs AS d SET deployment_overrides =
  ( SELECT COALESCE(jsonb_agg(jsonb_build_array("do".key, jsonb_build_object('tag', 'ValueAdded', 'contents', "do".value))), jsonb_build_array())
    FROM deployment_log_overrides as "do"
    WHERE d.id = "do".deployment_log_id AND scope = 'DeploymentScope'
  );
ALTER TABLE deployment_logs ALTER COLUMN deployment_overrides SET NOT NULL;

ALTER TABLE deployment_logs ADD COLUMN app_overrides jsonb;
UPDATE deployment_logs AS d SET app_overrides =
  ( SELECT COALESCE(jsonb_agg(jsonb_build_array("do".key, jsonb_build_object('tag', 'ValueAdded', 'contents', "do".value))), jsonb_build_array())
    FROM deployment_log_overrides as "do"
    WHERE d.id = "do".deployment_log_id AND scope = 'ApplicationScope'
  );
ALTER TABLE deployment_logs ALTER COLUMN app_overrides SET NOT NULL;

DROP TABLE deployment_log_overrides;


COMMIT;
