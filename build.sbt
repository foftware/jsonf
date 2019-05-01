import Dependencies._
import CommonSettings._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "org.foftware"
ThisBuild / organizationName := "foftware"

lazy val root = (project in file("."))
  .settings(
    initialCommands in console :=
			"""
        |import io.circe._
        |import io.circe.syntax._
				|import io.circe.validator._
				|import io.circe.validator.literal._
				|import io.circe.validator.internal._
				""".stripMargin
  ).dependsOn(literal, `scalatest-validator`, validator)

lazy val literal = (project in file("./modules/literal"))
  .settings(
    libraryDependencies := literalDependencies,
    name := "circe-validator-literal",
    scalacOptions := compilerFlags
  ).dependsOn(validator)

lazy val `scalatest-validator` = (project in file("./modules/scalatest-circe-validator/"))
  .settings(
    libraryDependencies := scalatestValidatorDependencies,
    name := "scalatest-circe-validator",
    scalacOptions := compilerFlags
  ).dependsOn(validator)

lazy val validator = (project in file("./modules/validator"))
  .settings(
    libraryDependencies := validatorDependencies,
    name := "circe-validator",
    scalacOptions := compilerFlags
  )
