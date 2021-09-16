.PHONY: all
all: interpreter

stack:
	make/stack.sh

interpreter: stack
	make/interpreter.sh

.PHONY: test
test: interpreter
	make/test.sh

.PHONY: zip
zip:
	make/zip.sh
