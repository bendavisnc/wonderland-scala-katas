val scala3Version = "3.7.4"

lazy val root = project
  .in(file("."))
  .settings(
    name                                   := "magicsquare",
    version                                := "0.0.0-SNAPSHOT",
    scalaVersion                           := scala3Version,
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.9.0",
    libraryDependencies += "org.scalameta" %% "munit"     % "1.0.0" % Test,
    Compile / compile := (Compile / compile)
      .dependsOn(Compile / scalafmt)
      .value,
    Test / compile := (Test / compile)
      .dependsOn(Test / scalafmt)
      .value
  )
