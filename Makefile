build-local:
	@sbt publishLocal

publish:
	@sbt clean && sbt test && sbt publishSigned

cover:
	@sbt clean coverage test && sbt coverageReport
