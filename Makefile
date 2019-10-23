
.PHONY: test-data
test-data: ## Generate some example tasks
	for n in $$(seq 10); do task add "Example task $${n}"; done

.PHONY: test
test: fetch-deps ## Run ERT test suite
	emacs -batch -l dash.el -l transient.el  -l taskwarrior.el -l taskwarrior-test.el -f ert-run-tests-batch-and-exit

.PHONY: fetch-deps
fetch-deps: ## Fetch required dependencies
	curl -fsSkL --retry 9 --retry-delay 9 -O "https://raw.githubusercontent.com/magit/transient/master/lisp/transient.el"
	curl -fsSkL --retry 9 --retry-delay 9 -O "https://raw.githubusercontent.com/magnars/dash.el/master/dash.el"
