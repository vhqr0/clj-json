.PHONY: test
test:
	lein test

.PHONY: check
check:
	lein check

.PHONY: cljs-test
cljs-test:
	shadow-cljs compile test
