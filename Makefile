.PHONY: all
all: interpreter

stack:
	wget https://get.haskellstack.org/stable/linux-x86_64.tar.gz;\
	tar xvf linux-x86_64.tar.gz;\
	rsync stack-2.7.1-linux-x86_64/stack .

.PHONY: interpreter_
interpreter_: stack
	./stack build

interpreter: interpreter_
	rsync "$(./stack exec -- whereis ish-v2 | awk '{print $2}')" interpreter
