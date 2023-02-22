BINARY := $(CURDIR)/rusch

rusch: src/*
	cargo build --profile optimized
	cp -f target/optimized/rusch $(BINARY)

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
integration-test:
	cd examples/the-little-schemer/ && cargo run -- run-all.scm

.PHONY: benchmark
benchmark: rusch
	cd examples/the-little-schemer/ && \
		hyperfine -m 200 --warmup 10 \
			'../../rusch run-all.scm' \
			'loco run-all.scm' \
			'gosch run-all.scm'

.PHONY: lines
lines:
	@ find . -type f -name "*.rs" -exec awk '1;/#[cfg\(test\)]/{exit}' {} \; | grep . | wc -l

.PHONY: clean
clean:
	rm -rf rusch
	rm -rf target
	rm -rf Cargo.lock
