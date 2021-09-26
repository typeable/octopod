-- Revert octopod:move_tag_to_overrides from pg
BEGIN;
ALTER TABLE deployments
ADD COLUMN tag text;
UPDATE deployments AS d
SET tag = COALESCE(
    (
      WITH appOvs AS (
        SELECT jsonb_array_elements(d."app_overrides") AS res
      )
      SELECT a.res->1->>'contents'
      FROM appOvs AS a
      WHERE (a.res->>0) = 'image.tag'
    ),
    ''
  );
UPDATE deployments AS d
SET app_overrides = COALESCE(
    (
      WITH appOvs AS (
        SELECT jsonb_array_elements(d."app_overrides") AS res
      )
      SELECT jsonb_agg(a.res)
      FROM appOvs AS a
      WHERE (a.res->>0) != 'image.tag'
    ),
    '[]'
  );
ALTER TABLE deployments
ALTER COLUMN tag
SET NOT NULL;
--
ALTER TABLE deployment_logs
ADD COLUMN tag text;
UPDATE deployment_logs AS d
SET tag = COALESCE(
    (
      WITH appOvs AS (
        SELECT jsonb_array_elements(d."app_overrides") AS res
      )
      SELECT a.res->1->>'contents'
      FROM appOvs AS a
      WHERE (a.res->>0) = 'image.tag'
    ),
    ''
  );
UPDATE deployment_logs AS d
SET app_overrides = COALESCE(
    (
      WITH appOvs AS (
        SELECT jsonb_array_elements(d."app_overrides") AS res
      )
      SELECT jsonb_agg(a.res)
      FROM appOvs AS a
      WHERE (a.res->>0) != 'image.tag'
    ),
    '[]'
  );
ALTER TABLE deployment_logs
ALTER COLUMN tag
SET NOT NULL;
COMMIT;
