.PHONY: docs mermaid plantuml

docs: mermaid plantuml

mermaid:
	# to install mmdc use https://github.com/mermaidjs/mermaid.cli
	#
	# mmdc does not support stateDiagram-v2 (technical-architecture-deployment-statuses-fsm.mmd),
	# use https://mermaid-js.github.io/mermaid-live-editor to render it
	for src in `ls diagrams/src/*.mmd | grep -v technical-architecture-deployment-statuses-fsm.mmd`; do \
		name=`basename $$src .mmd`; \
		mmdc -i $$src -o "diagrams/images/$$name.png" --scale 4 --cssFile style.css; \
	done

plantuml:
	# to install plantuml go to https://plantuml.com/command-line
	#
	# plantuml takes output paths relative to input file
	for src in `ls diagrams/src/*.puml`; do \
		name=`basename $$src .mmd`; \
		plantuml -I $$src -o "../../diagrams/images" -tpng; \
	done

plantuml:
	# to install plantuml go to https://plantuml.com/command-line
	#
	# plantuml takes output paths relative to input file
	for src in `ls diagrams/src/*.puml`; do \
		name=`basename $$src .mmd`; \
		plantuml -I $$src -o "../../diagrams/images" -tpng; \
	done
