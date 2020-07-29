.PHONY: docs

docs:
	# for install mmdc use https://github.com/mermaidjs/mermaid.cli
	#
	# mmdc does not support stateDiagram-v2 (technical-architecture-staging-statuses-fsm.mmd),
	# use https://mermaid-js.github.io/mermaid-live-editor to render it
	for src in `ls diagrams/src/*.mmd | grep -v technical-architecture-staging-statuses-fsm.mmd`; do \
		name=`basename $$src .mmd`; \
		mmdc -i $$src -o "diagrams/images/$$name.png" -w 2048 --cssFile style.css; \
	done
