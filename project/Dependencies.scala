import sbt._

object Version {
  lazy val cats = "1.6.0"
  lazy val catsMtl = "0.5.0"
  lazy val circe = "0.11.1"
  lazy val jawn = "0.14.1"
  lazy val scalaTest = "3.0.5"
  lazy val scalaParserCombinators = "1.1.2"
}

object Dependencies {
  lazy val cats = "org.typelevel" %% "cats-core" % Version.cats
  lazy val catsMtl = "org.typelevel" %% "cats-mtl-core" % Version.catsMtl
  lazy val catsTestKit = "org.typelevel" %% "cats-testkit"  % Version.cats
  lazy val circeCore = "io.circe" %% "circe-core" % Version.circe
  lazy val circeGeneric = "io.circe" %% "circe-generic" % Version.circe
  lazy val circeLiteral = "io.circe" %% "circe-literal" % Version.circe
  lazy val jawnParser = "org.typelevel" %% "jawn-parser" % Version.jawn
  lazy val scalaParserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % Version.scalaParserCombinators
  lazy val scalaTest = "org.scalatest" %% "scalatest" % Version.scalaTest


  lazy val validatorDependencies = {
    lazy val compile = Seq(
      cats,
      catsMtl,
      circeCore,
      compilerPlugin("org.typelevel" %% "kind-projector" % "0.10.0"),
    )
    lazy val test = Seq(catsTestKit, circeGeneric, scalaTest) map (_ % Test)

    compile ++ test
  }

  lazy val literalDependencies = {
    lazy val compile = Seq(jawnParser)
    lazy val test = Seq(circeLiteral, scalaTest) map (_ % Test)

    compile ++ test
  }

  lazy val scalatestValidatorDependencies = {
    lazy val compile = Seq(scalaTest)
    lazy val test = Seq(circeLiteral, circeGeneric) map (_ % Test)

    compile ++ test
  }
}
