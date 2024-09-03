CREATE TABLE deployment (
    id SERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    created_at TIMESTAMPTZ NOT NULL
);
CREATE TABLE deployment_action (
    id SERIAL PRIMARY KEY,
    deployment_id INT NOT NULL REFERENCES deployment(id) ON DELETE CASCADE,
    created_at TIMESTAMPTZ NOT NULL,
    error TEXT
);
CREATE TABLE deployment_helm_override (
    id SERIAL PRIMARY KEY,
    deployment_action_id INT NOT NULL REFERENCES deployment_action(id) ON DELETE CASCADE,
    version VARCHAR(50)
);
CREATE TABLE deployment_helm_values_override (
    id SERIAL PRIMARY KEY,
    deployment_action_id INT NOT NULL REFERENCES deployment_action(id) ON DELETE CASCADE,
    action VARCHAR(255),
    key TEXT,
    value TEXT
);
CREATE TABLE deployment_status (
    id SERIAL PRIMARY KEY,
    deployment_id INT NOT NULL REFERENCES deployment(id) ON DELETE CASCADE,
    status VARCHAR(50) NOT NULL,
    is_pending BOOLEAN NOT NULL,
    checked_at TIMESTAMPTZ NOT NULL,
    updated_at TIMESTAMPTZ NOT NULL
);
