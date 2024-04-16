SHELL	= bash
BRANCH	= main

usage:
	@echo install
	@echo update

install:
	./install.sh

update: git-pull install

git-pull:
	@[ "$$(git rev-parse --abbrev-ref HEAD)" = "${BRANCH}" ] && git pull
