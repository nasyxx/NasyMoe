all:
	echo "do nothing."
.PHONY: all


build:
	stack build
.PHONY: build


build-web: build
	stack exec -- site build
.PHONY: build-web


open: build-web
	open "public/index.html"
.PHONY: open
