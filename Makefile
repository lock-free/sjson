build-local:
	@sbt publishLocal

publish:
	@sbt clean && sbt test && sbt publishSigned
