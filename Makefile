SHELL = /usr/bin/env bash
DOCS ?= docs.json


define feedback
	@echo -e "\\e[35m$(1)\\e[0m"
endef


.PHONY: all
all: compile docs test


.PHONY: compile
compile:
	$(call feedback,Compling elm package...)
	@elm make > /dev/null


.PHONY: docs
docs:
	$(call feedback,Creating $(DOCS)...)
	@elm make --docs=$(DOCS) > /dev/null


.PHONY: test
test:
	$(call feedback,Running unit/fuzz tests...)
	@npm run --silent tests
	$(call feedback,Running doctests...)
	@elm-doctest


.PHONY: diagram
diagram:
	$(call feedback,Creating diagram.svg...)
	@dot -Tsvg -o diagram.svg diagram.gv
