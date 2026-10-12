name         := "doublets"
version      := "0.0.0-SNAPSHOT"
scalaVersion := "3.8.4"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core"        % "2.9.0",
  "com.lihaoyi"   %% "pprint"           % "0.9.6",
  "io.circe"      %% "circe-parser"     % "0.14.16",
  "org.scalameta" %% "munit"            % "1.0.0" % Test,
  "org.scalameta" %% "munit-scalacheck" % "1.2.0" % Test
)

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Werror",
  "-Wunused:all",
  "-Wconf:msg=No class.*defined in the compilation unit:s" // all `doublets.scala` as a package style helper class
)