ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.2"

lazy val root = (project in file("."))
  .settings(
    name := "third-cats",
    scalacOptions ++= Seq("-source:future-migration", "-Xfatal-warnings", "-explain" ),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12" % "test",
    libraryDependencies += "org.typelevel" % "cats-core_3" % "2.7.0",
    libraryDependencies += "org.typelevel" % "cats-free_3" % "2.7.0"

  )
