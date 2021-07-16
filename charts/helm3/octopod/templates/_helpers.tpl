{{/*
Expand the name of the chart.
*/}}
{{- define "octopod.name" -}}
{{- default .Chart.Name .Values.nameOverride | trunc 63 | trimSuffix "-" }}
{{- end }}

{{/*
Create a default fully qualified app name.
We truncate at 63 chars because some Kubernetes name fields are limited to this (by the DNS naming spec).
If release name contains chart name it will be used as a full name.
*/}}
{{- define "octopod.fullname" -}}
{{- if .Values.fullnameOverride }}
{{- .Values.fullnameOverride | trunc 63 | trimSuffix "-" }}
{{- else }}
{{- $name := default .Chart.Name .Values.nameOverride }}
{{- if contains $name .Release.Name }}
{{- .Release.Name | trunc 63 | trimSuffix "-" }}
{{- else }}
{{- printf "%s-%s" .Release.Name $name | trunc 63 | trimSuffix "-" }}
{{- end }}
{{- end }}
{{- end }}

{{/*
Create chart name and version as used by the chart label.
*/}}
{{- define "octopod.chart" -}}
{{- printf "%s-%s" .Chart.Name .Chart.Version | replace "+" "_" | trunc 63 | trimSuffix "-" }}
{{- end }}

{{/*
Common labels
*/}}
{{- define "octopod.labels" -}}
helm.sh/chart: {{ include "octopod.chart" . }}
{{ include "octopod.selectorLabels" . }}
{{- if .Chart.AppVersion }}
app.kubernetes.io/version: {{ .Chart.AppVersion | quote }}
{{- end }}
app.kubernetes.io/managed-by: {{ .Release.Service }}
{{- end }}

{{/*
Selector labels
*/}}
{{- define "octopod.selectorLabels" -}}
app.kubernetes.io/name: {{ include "octopod.name" . }}
app.kubernetes.io/instance: {{ .Release.Name }}
{{- end }}

{{/*
Create the name of the service account to use
*/}}
{{- define "octopod.serviceAccountName" -}}
{{- if .Values.serviceAccount.create }}
{{- default (include "octopod.fullname" .) .Values.serviceAccount.name }}
{{- else }}
{{- default "default" .Values.serviceAccount.name }}
{{- end }}
{{- end }}

{{- define "controlScriptsPath" -}}
/utils
{{- end }}

{{- define "octopodAppAuthSecretName" -}}
{{- printf "%s-app-auth-secret" (include "octopod.fullname" .) }}
{{- end }}

{{- define "httpScheme" -}}
{{- if .Values.ingress.tls.enabled -}}
https
{{- else -}}
http
{{- end }}
{{- end }}

{{- define "wsScheme" -}}
{{- if .Values.ingress.tls.enabled -}}
wss
{{- else -}}
ws
{{- end }}
{{- end }}

{{- define "postgresqlHost" -}}
{{ .Release.Name }}-postgresql
{{- end }}

{{- define "postgresqlSecretName" -}}
{{ .Release.Name }}-postgresql
{{- end }}

{{- define "wsIngressHost" -}}
{{- if .Values.ingress.ws.host -}}
{{ .Values.ingress.ws.host }}
{{- else -}}
octopod-ws.{{ .Values.octopod.baseDomain }}
{{- end }}
{{- end }}

{{- define "uiIngressHost" -}}
{{- if .Values.ingress.ui.host -}}
{{ .Values.ingress.ui.host }}
{{- else -}}
octopod.{{ .Values.octopod.baseDomain }}
{{- end }}
{{- end }}

{{- define "powerAppIngressHost" -}}
{{- if .Values.ingress.powerApp.host -}}
{{ .Values.ingress.powerApp.host }}
{{- else -}}
octopod-powerapp.{{ .Values.octopod.baseDomain }}
{{- end }}
{{- end }}

{{- define "appIngressHost" -}}
{{- if .Values.ingress.app.host -}}
{{ .Values.ingress.app.host }}
{{- else -}}
octopod-app.{{ .Values.octopod.baseDomain }}
{{- end }}
{{- end }}