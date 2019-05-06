import Dependencies._
import CommonSettings._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "org.foftware"
ThisBuild / organizationName := "foftware"

addCommandAlias("lint", ";scalafmtCheck;test:scalafmtCheck")
addCommandAlias("fmt", ";scalafmt;test:scalafmt")

lazy val root = (project in file("."))
  .settings(commonSettings)
  .settings(
    initialCommands in console :=
			"""
				|import io.circe._
        |import io.circe.syntax._
				|import io.circe.validator._
				|import io.circe.validator.literal._
				|import io.circe.validator.internal._
				""".stripMargin,
		scalacOptions in (Compile, console) --= Seq("-Xfatal-warnings")
  ).dependsOn(literal, `scalatest-validator`, validator)
  .aggregate(literal, `scalatest-validator`, validator)

lazy val literal = (project in file("./modules/literal"))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= literalDependencies,
    name := "circe-validator-literal",
  ).dependsOn(validator)

lazy val `scalatest-validator` = (project in file("./modules/scalatest-circe-validator/"))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= scalatestValidatorDependencies,
    name := "scalatest-circe-validator",
  ).dependsOn(validator)

lazy val validator = (project in file("./modules/validator"))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= validatorDependencies,
    name := "circe-validator",
  )
