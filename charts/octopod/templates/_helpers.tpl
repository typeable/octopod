{{/*
Set dbname
*/}}
{{- define "dbname" -}}
{{- $dbname_release := .Release.Name | replace "." "_" | replace "-" "_"  -}}
{{- .Values.dbname | default $dbname_release }}
{{- end -}}
