export GPG_TTY=$(shell tty)

build-local:
	@sbt publishLocal

publish:
	@sbt clean && sbt test && sbt publishSigned

Release:
	@make publish && sbt sonatypeRelease

cover:
	@sbt clean coverage test && sbt coverageReport
