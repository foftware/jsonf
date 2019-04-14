import sbt._

object Version {
  lazy val cats = "1.6.0"
  lazy val catsMtl = "0.5.0"
  lazy val circe = "0.11.1"
  lazy val jawn = "0.14.1"
  lazy val scalaTest = "3.0.5"
}

object Dependencies {
  lazy val cats = "org.typelevel" %% "cats-core" % Version.cats
  lazy val catsMtl = "org.typelevel" %% "cats-mtl-core" % Version.catsMtl
  lazy val circeCore = "io.circe" %% "circe-core" % Version.circe
  lazy val scalaTest = "org.scalatest" %% "scalatest" % Version.scalaTest
  lazy val jawnParser = "org.typelevel" %% "jawn-parser" % Version.jawn

  lazy val validatorDependencies = Seq(
    cats,
    catsMtl,
    circeCore
  )

  lazy val literalDependencies = Seq(jawnParser)

  lazy val scalatestValidatorDependencies = Seq(scalaTest)
}
