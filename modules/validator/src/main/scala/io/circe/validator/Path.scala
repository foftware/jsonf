package io.circe.validator

sealed trait PathStep

object PathStep {

  final case class Key(key: String)  extends PathStep
  final case class Index(index: Int) extends PathStep
}
