export IDRIS2 ?= idris2

lib_pkg = todomvc.ipkg

.PHONY: all
all: lib

.PHONY: clean-install
clean-install: clean install

.PHONY: clean-install-with-src
clean-install-with-src: clean install

.PHONY: lib
lib:
	${IDRIS2} --build ${lib_pkg}

.PHONY: page
page: lib
	mkdir -p js && cp build/exec/app.js js/app.js

.PHONY: clean
clean:
	${IDRIS2} --clean ${lib_pkg}
	${RM} -r build

.PHONY: develop
develop:
	find -regextype posix-extended -regex ".*\.(idr|md)" | entr -d idris2 --typecheck ${lib_pkg}
