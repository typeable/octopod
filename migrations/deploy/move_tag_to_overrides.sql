-- Deploy octopod:move_tag_to_overrides to pg

BEGIN;


UPDATE deployments as d
  SET app_overrides = d.app_overrides ||
    jsonb_build_array(jsonb_build_array('image.tag', jsonb_build_object('tag', 'ValueAdded', 'contents', d.tag)));

ALTER TABLE "deployments" DROP COLUMN "tag";

UPDATE deployment_logs as d
  SET app_overrides = d.app_overrides ||
    jsonb_build_array(jsonb_build_array('image.tag', jsonb_build_object('tag', 'ValueAdded', 'contents', d.tag)));

ALTER TABLE "deployment_logs" DROP COLUMN "tag";

COMMIT;
