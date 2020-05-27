###############################################################################
# run-workflow
###############################################################################
.PHONY: run-workflow
run-workflow:
	make run-xform
	make run-git-push


###############################################################################
# run-xform
###############################################################################
.PHONY: run-xform
run-xform:
	cd .github/actions/run-xform; make dc-up; cd -


.PHONY: run-xform-build
run-xform-build:
	cd .github/actions/run-xform; make dc-up-build; cd -


.PHONY: run-xform-build-no-cache
run-xform-build-no-cache:
	cd .github/actions/run-xform; make dc-build-no-cache; cd -


###############################################################################
# git-push
###############################################################################
.PHONY: run-git-push
run-git-push:
	cd .github/actions/git-push; make dc-up; cd -


.PHONY: run-git-push-build
run-git-push-build:
	cd .github/actions/git-push; make dc-up-build; cd -


.PHONY: run-git-push-build-no-cache
run-git-push-build-no-cache:
	cd .github/actions/git-push; make dc-build-no-cache; cd -

