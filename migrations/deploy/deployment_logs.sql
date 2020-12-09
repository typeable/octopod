-- Deploy octopod:deployment_logs to pg

BEGIN;

CREATE TYPE actions AS ENUM ('create', 'edit', 'update');

CREATE TABLE deployment_logs (
	id BIGSERIAL PRIMARY KEY,
	deployment_id BIGINT NOT NULL REFERENCES deployments(id),
	action actions,
	tag TEXT,
	envs TEXT,
	exit_code SMALLINT NOT NULL,
	created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

COMMIT;
