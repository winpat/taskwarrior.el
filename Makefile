.PHONY: test-data

test-data: ## Generate some example tasks
	for n in $$(seq 10); do task add "Example task $${n}"; done
