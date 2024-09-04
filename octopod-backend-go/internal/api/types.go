package api

import "time"

type Link struct {
	Name string `json:"name"`
	URL  string `json:"url"`
}

type OverrideAction string

const (
	ValueAdd    OverrideAction = "ValueAdd"
	ValueDelete OverrideAction = "ValueDelete"
)

type Override struct {
	Name           string         `json:"name"`
	OverrideAction OverrideAction `json:"action"`
	Value          string         `json:"value"`
}

type DeploymentData struct {
	Name                 string     `json:"name"`
	ValuesOverride       []Override `json:"values_overrides"`
	ChartVersionOverride string     `json:"chart_version_override"`
}

type Deployment struct {
	DeploymentData DeploymentData `json:"deployment_data"`
	Status         Status         `json:"status"`
	Pending        bool           `json:"pending"`
	Links          []Link         `json:"links"`
}

type Action string

const (
	Create  Action = "Create"
	Archive Action = "Archive"
	Update  Action = "Update"
	Restore Action = "Restore"
)

type Status string

const (
	Creating  Action = "Creating"
	Running   Action = "Running"
	Archiving Action = "Archiving"
	Archived  Action = "Archived"
	Updating  Action = "Updating"
	Restoring Action = "Restoring"
)

type DeploymentAction struct {
	Action               Action     `json:"action"`
	ValuesOverride       []Override `json:"values_overrides"`
	ChartVersionOverride string     `json:"chart_version_override"`
	Error                string     `json:"error"`
	CreatedAt            time.Time  `json:"created_at"`
}

type FullStatus struct {
	Pending bool   `json:"pending"`
	Status  string `json:"status"`
}
