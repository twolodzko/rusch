BINARY := "$(pwd)/rusch"

test: lint unit-test integration-test

rusch:
	cargo build --profile optimized
	cp -f target/optimized/rusch {{BINARY}}

repl:
	@ cargo run --quiet

lint:
	cargo clippy

unit-test:
	cargo test

integration-test:
	cd examples/the-little-schemer/ && cargo run -- run-all.scm

benchmark: rusch
	cd examples/the-little-schemer/ && \
		hyperfine -m 200 --warmup 10 \
			'../../rusch run-all.scm' \
			'loco run-all.scm' \
			'gosch run-all.scm'

install:
	cargo install --profile optimized --path .

lines:
	@ find . -type f -name "*.rs" -exec awk '1;/#[cfg\(test\)]/{exit}' {} \; | grep . | wc -l

clean:
	rm -rf rusch
	rm -rf target
	rm -rf Cargo.lock
