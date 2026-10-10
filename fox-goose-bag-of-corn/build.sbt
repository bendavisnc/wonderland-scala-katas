name := "fox-goose-bag-of-corn"
version := "0.0.0-SNAPSHOT"
scalaVersion := "3.8.4"

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "pprint" % "0.9.6",
  "org.scalameta" %% "munit" % "1.0.0" % Test,
  "org.scalameta" %% "munit-scalacheck" % "1.2.0" % Test
)
