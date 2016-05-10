
build: ## build
	echo stack build

test: FORCE ## test
	stack test --ghc-options -fprof-auto --test-arguments +RTS -N -RTS \
	'--hide-successes -j8'
	stack install

clean: FORCE ## clean all the things
	sh clean.sh

work: ## open all files in editor
	emacs -nw *.cabal Makefile \
	battle-plan.org src/Uasm/*.hs \
	src/Beta/*.hs \
	app/*.hs \
	test/*.hs \
	test/TestBeta/*.hs

setup:
	touch battle-plan.org
	mkdir -p design

add: clean
	git add -A :/

commit:
	git commit -a


# http://marmelab.com/blog/2016/02/29/auto-documented-makefile.html
.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk \
	'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

FORCE:

