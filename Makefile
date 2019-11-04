ELPA_DEPENDENCIES=package-lint transient dash

ELPA_ARCHIVES=melpa-stable gnu

TEST_ERT_FILES		= $(wildcard test/*.el)
LINT_CHECKDOC_FILES	= "taskwarrior.el" ${TEST_ERT_FILES}
LINT_PACKAGE_LINT_FILES	= ${LINT_CHECKDOC_FILES}
LINT_COMPILE_FILES	= ${LINT_CHECKDOC_FILES}

makel.mk: # Download makel
	@if [ -f ../makel/makel.mk ]; then \
		ln -s ../makel/makel.mk .; \
	else \
		curl \
		--fail --silent --show-error --insecure --location \
		--retry 9 --retry-delay 9 \
		-O https://gitlab.petton.fr/DamienCassou/makel/raw/v0.5.3/makel.mk; \
	fi

.PHONY: test-data
test-data: ## Generate some example tasks
	for n in $$(seq 10); do task add "Example task $${n}"; done

# .PHONY: fetch-deps
# fetch-deps: ## Fetch required dependencies
# 	curl -fsSkL --retry 9 --retry-delay 9 -O "https://raw.githubusercontent.com/magit/transient/master/lisp/transient.el"
# 	curl -fsSkL --retry 9 --retry-delay 9 -O "https://raw.githubusercontent.com/magnars/dash.el/master/dash.el"

# Include makel.mk if present
-include makel.mk
