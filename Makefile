all:
	echo "do nothing."
.PHONY: all


build:
	stack build
.PHONY: build


build-web: build
	stack exec -- site rebuild
.PHONY: build-web


open: build-web
	open "public/index.html"
.PHONY: open


clean-web:
	stack exec -- site clean
.PHONY: clean-web


watch: clean-web
	stack exec -- site watch
.PHONY: watch


server: build
	stack exec -- site server
.PHONY: server
