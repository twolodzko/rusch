
.PHONY: validate
validate: lint test integration-test

.PHONY: test
test:
	cargo test

.PHONY: integration-test
integration-test: rusch
	cd examples/the-little-schemer/ && ../../rusch run-all.scm

.PHONY: lint
lint:
	cargo clippy

.PHONY: lines
lines:
	@ find . -type f -name "*.rs" -exec awk '1;/#[cfg\(test\)]/{exit}' {} \; | grep . | wc -l

rusch: src/*
	cargo build --release
	cp -f target/release/rusch .

.PHONY: repl
repl:
	@ cargo run
