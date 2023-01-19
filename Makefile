BINARY := $(CURDIR)/rusch

rusch: src/*
	cargo build --release
	cp -f target/release/rusch $(BINARY)

.PHONY: repl
repl:
	@ cargo run --quiet

.PHONY: test
test: lint unit-test integration-test

.PHONY: lint
lint:
	cargo clippy

.PHONY: unit-test
unit-test:
	cargo test

.PHONY: integration-test
integration-test: rusch
	cd examples/the-little-schemer/ && $(BINARY) run-all.scm

.PHONY: lines
lines:
	@ find . -type f -name "*.rs" -exec awk '1;/#[cfg\(test\)]/{exit}' {} \; | grep . | wc -l
