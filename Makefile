SHELL = /usr/bin/env bash


.PHONY: check
check:
	@find $(PWD)/src -type f -name '*.elm' -exec elm make {} > /dev/null \;
	@elm make --docs="$$(mktemp --suffix .json)"
	@elm-doctest


.PHONY: docs
docs:
	@elm make --docs=docs.json


.PHONY: diagram
diagram:
	@dot -Tsvg -o diagram.svg diagram.gv
