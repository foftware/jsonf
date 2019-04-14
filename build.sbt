import Dependencies._
import CommonSettings._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    initialCommands in console :=
			"""
        |import io.circe._
        |import io.circe.syntax._
				|import io.circe.validator._
				|import io.circe.validator.literal._
				""".stripMargin
  ).dependsOn(literal, validator)

lazy val literal = (project in file("./modules/literal"))
  .settings(
    libraryDependencies := literalDependencies,
    name := "circe-validator-literal",
    scalacOptions := compilerFlags
  ).dependsOn(validator)

lazy val validator = (project in file("./modules/validator"))
  .settings(
    libraryDependencies := validatorDependencies,
    name := "circe-validator",
    scalacOptions := compilerFlags
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
