import Dependencies._

lazy val scala213 = Version.scalaVersion
lazy val scala212 = "2.12.10"
lazy val supportedScalaVersions = List(scala213, scala212)

ThisBuild / scalaVersion  := scala213
ThisBuild / organization  := "com.github.foftware"
ThisBuild / startYear     := Some(2019)
ThisBuild / licenses      := Seq("MIT" -> url("http://opensource.org/licenses/MIT"))
ThisBuild / description   := "JSON template"

addCommandAlias("lint", ";scalafmtCheck;test:scalafmtCheck")
addCommandAlias("fmt", ";scalafmt;test:scalafmt")

lazy val credentialSettings = Seq(
  // For Travis CI - see http://www.cakesolutions.net/teamblogs/publishing-artefacts-to-oss-sonatype-nexus-using-sbt-and-travis-ci
	credentials ++= (for {
    username <- Option(System.getenv().get("SONATYPE_USERNAME"))
    password <- Option(System.getenv().get("SONATYPE_PASSWORD"))
  } yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq
)

lazy val publishSettings = Seq(
	homepage := Some(url("https://github.com/foftware/jsont")),
 	licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
 	scmInfo := Some(ScmInfo(url("https://github.com/foftware/jsont"), "scm:git:git@github.com:foftware/jsont.git")),
 	autoAPIMappings := true,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := Function.const(false),
	useGpg := true,
 	pomExtra := (
  	<developers>
      <developer>
        <id>kidonm</id>
        <name>Marek Kidoň</name>
        <url>https://github.com/kidonm</url>
      </developer>
      <developer>
        <id>fokot</id>
        <name>František Kocun</name>
        <url>https://github.com/fokot</url>
      </developer>
    </developers>
	),
	publishTo := {
  	val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "/content/repositories/snapshots")
    else
      Some("releases" at nexus + "/service/local/staging/deploy/maven2")
  },
	releasePublishArtifactsAction := PgpKeys.publishSigned.value
) ++ credentialSettings

lazy val noPublishSettings = Seq(
	skip in publish := true
)

lazy val commonSettings = Seq(
	scalacOptions := Seq(
    "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
    "-encoding", "utf-8",                // Specify character encoding used by source files.
    "-explaintypes",                     // Explain type errors in more detail.
    "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
    "-language:existentials",            // Existential types (besides wildcard types) can be written and inferred
    "-language:experimental.macros",     // Allow macro definition (besides implementation and application)
    "-language:higherKinds",             // Allow higher-kinded types
    "-language:implicitConversions",     // Allow definition of implicit functions called views
    "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
    "-Xcheckinit",                       // Wrap field accessors to throw an exception on uninitialized access.
    "-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
    "-Xlint:adapted-args",               // Warn if an argument list is modified to match the receiver.
    "-Xlint:constant",                   // Evaluation of a constant arithmetic expression results in an error.
    "-Xlint:delayedinit-select",         // Selecting member of DelayedInit.
    "-Xlint:doc-detached",               // A Scaladoc comment appears to be detached from its element.
    "-Xlint:inaccessible",               // Warn about inaccessible types in method signatures.
    "-Xlint:infer-any",                  // Warn when a type argument is inferred to be `Any`.
    "-Xlint:missing-interpolator",       // A string literal appears to be missing an interpolator id.
    "-Xlint:nullary-override",           // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Xlint:nullary-unit",               // Warn when nullary methods return Unit.
    "-Xlint:option-implicit",            // Option.apply used implicit view.
    "-Xlint:package-object-classes",     // Class or object defined in package object.
    "-Xlint:poly-implicit-overload",     // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow",             // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align",                // Pattern sequence wildcard must align with sequence component.
    "-Xlint:type-parameter-shadow",      // A local type parameter shadows a type already in scope.
    "-Ywarn-dead-code",                  // Warn when dead code is identified.
    "-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
    "-Ywarn-numeric-widen",              // Warn when numerics are widened.
    "-Ywarn-unused:implicits",           // Warn if an implicit parameter is unused.
    "-Ywarn-unused:imports",             // Warn if an import selector is not referenced.
    "-Ywarn-unused:locals",              // Warn if a local definition is unused.
    "-Ywarn-unused:params",              // Warn if a value parameter is unused.
    "-Ywarn-unused:patvars",             // Warn if a variable bound in a pattern is unused.
    "-Ywarn-unused:privates",            // Warn if a private member is unused.
    "-Ywarn-value-discard",               // Warn when non-Unit expression results are unused.
    ) ++
    (CrossVersion.partialVersion(scalaVersion.value) match {
      case List((2, 13)) => Seq("-Ymacro-annotations")
      case _ => Seq()
    })
  ,
	wartremoverErrors := Warts.all,
	headerMappings := headerMappings.value + (HeaderFileType.scala -> HeaderCommentStyle.cppStyleLineComment),
	headerLicense := Some(HeaderLicense.Custom(
    """|Copyright (c) 2019 Marek Kidoň and František Kocun
       |This software is licensed under the MIT License (MIT).
       |For more information see LICENSE or https://opensource.org/licenses/MIT
       """.stripMargin)),
  crossScalaVersions := supportedScalaVersions,
)

lazy val root = (project in file("."))
  .settings(commonSettings)
	.settings(publishSettings) // To publish all aggregated projects
	.settings(noPublishSettings) // Exclude root module itself from being published
  .settings(
    libraryDependencies += Dependencies.circeLiteral,
    initialCommands in console :=
			"""
				|import io.circe._
        |import io.circe.syntax._
				|import io.circe.literal._
				|import io.circe.validator._
				|import io.circe.validator.literal._
				|import io.circe.validator.internal._
				""".stripMargin,
		scalacOptions in (Compile, console) --= Seq("-Xfatal-warnings"),
		scalacOptions in (Compile, doc) --= Seq("-Xfatal-warnings"),
    // crossScalaVersions must be set to Nil on the aggregating project
    crossScalaVersions := Nil,
  ).dependsOn(core, literal, `jsont-scalatest`)
  .aggregate(core, literal, `jsont-scalatest`)

lazy val literal = (project in file("./modules/literal"))
  .settings(commonSettings)
	.settings(publishSettings)
  .settings(
    libraryDependencies ++= literalDependencies,
    name := "jsont-literal",
  ).dependsOn(core)

lazy val `jsont-scalatest` = (project in file("./modules/jsont-scalatest/"))
  .settings(commonSettings)
	.settings(publishSettings)
  .settings(
    libraryDependencies ++= scalatestValidatorDependencies,
    name := "jsont-scalatest",
  ).dependsOn(core, literal)

lazy val core = (project in file("./modules/core"))
  .settings(commonSettings)
	.settings(publishSettings)
  .settings(
    libraryDependencies ++= validatorDependencies,
    name := "jsont-core",
  )
